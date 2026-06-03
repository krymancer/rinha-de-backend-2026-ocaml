/* SIMD i16 squared-Euclidean distance kernel for the kNN leaf scan.

   The leaf scan dominates the hard-query tail (queries that miss the early-exit
   and probe many partition buckets) — exactly the queries that set p99. Each
   reference point is 14 i16 dims; the scalar OCaml loop does up to 14 iterations
   per point. Here the full 14-dim squared distance is computed with SSE4.1 in a
   handful of instructions per point.

   Exactness — the result is the exact integer squared distance, bit-identical to
   the scalar path. tests/test_knn.ml compares this (via fraud_count_with) against
   a brute-force exact kNN and asserts zero distance mismatch.

     - No overflow: a single dim diff is in [-20000, 20000], squared <= 4e8; the
       full 14-dim sum is <= 5.6e9, beyond int32. The horizontal sum is widened
       to int64 before any value can exceed int32, and stored into an OCaml-int
       (63-bit) Bigarray. Safe.
     - No over-read: a point is 14*2 = 28 bytes. We load dims 0..7 (16 B), dims
       8..11 (8 B), then dims 12..13 (4 B) — exactly 28 bytes. Never reads into
       the next point, so even the final point in the table is safe regardless of
       page alignment. */

#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <stdint.h>
#include <string.h>

#define KDIM 14

#if defined(__SSE4_1__)
#include <smmintrin.h>

/* squared euclidean over the 14 i16 dims at [p] against the query held in
   (qa = dims 0..7, qb = dims 8..13 in lanes 0..5, lanes 6,7 = 0). */
static inline int64_t dist14(const int16_t *p, __m128i qa, __m128i qb) {
    __m128i a = _mm_loadu_si128((const __m128i *)p);          /* dims 0..7  */
    __m128i b = _mm_loadl_epi64((const __m128i *)(p + 8));    /* dims 8..11 -> lanes 0..3 */
    int32_t last2;
    memcpy(&last2, p + 12, 4);                                /* dims 12,13 */
    b = _mm_or_si128(b, _mm_slli_si128(_mm_cvtsi32_si128(last2), 8)); /* -> lanes 4,5 */

    __m128i da = _mm_sub_epi16(a, qa);
    __m128i db = _mm_sub_epi16(b, qb);
    __m128i pa = _mm_madd_epi16(da, da);   /* 4x i32, each <= 8e8 */
    __m128i pb = _mm_madd_epi16(db, db);   /* 4x i32 */
    __m128i s  = _mm_add_epi32(pa, pb);    /* 4x i32, each <= 1.6e9 (< int32 max) */

    __m128i lo  = _mm_cvtepi32_epi64(s);                       /* lanes 0,1 -> i64 */
    __m128i hi  = _mm_cvtepi32_epi64(_mm_srli_si128(s, 8));    /* lanes 2,3 -> i64 */
    __m128i s64 = _mm_add_epi64(lo, hi);                       /* 2x i64 */
    return (int64_t)_mm_extract_epi64(s64, 0) + (int64_t)_mm_extract_epi64(s64, 1);
}

CAMLprim value fraud_simd_leaf_dists(value vvecs, value vq16, value vstart,
                                     value vlen, value vout) {
    const int16_t *vecs = (const int16_t *)Caml_ba_data_val(vvecs);
    const int16_t *q    = (const int16_t *)Caml_ba_data_val(vq16);
    intnat *out         = (intnat *)Caml_ba_data_val(vout);
    long start = Long_val(vstart);
    long len   = Long_val(vlen);

    __m128i qa = _mm_loadu_si128((const __m128i *)q);          /* dims 0..7  */
    __m128i qb = _mm_loadu_si128((const __m128i *)(q + 8));    /* dims 8..13 (lanes 6,7 = 0) */

    for (long i = 0; i < len; i++)
        out[i] = (intnat)dist14(vecs + (start + i) * KDIM, qa, qb);
    return Val_unit;
}

#else  /* portable scalar fallback (identical result) */

CAMLprim value fraud_simd_leaf_dists(value vvecs, value vq16, value vstart,
                                     value vlen, value vout) {
    const int16_t *vecs = (const int16_t *)Caml_ba_data_val(vvecs);
    const int16_t *q    = (const int16_t *)Caml_ba_data_val(vq16);
    intnat *out         = (intnat *)Caml_ba_data_val(vout);
    long start = Long_val(vstart);
    long len   = Long_val(vlen);
    for (long i = 0; i < len; i++) {
        const int16_t *p = vecs + (start + i) * KDIM;
        int64_t acc = 0;
        for (int d = 0; d < KDIM; d++) {
            int64_t df = (int64_t)p[d] - (int64_t)q[d];
            acc += df * df;
        }
        out[i] = (intnat)acc;
    }
    return Val_unit;
}

#endif
