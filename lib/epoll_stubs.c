/* Minimal epoll bindings for the fraud server's event loop.
   OCaml's Unix.select rebuilds fd_sets and allocates lists on every call,
   which caps single-core throughput. epoll_wait is O(ready) and writes into
   caller-preallocated int arrays here, so the hot loop allocates nothing. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/threads.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <string.h>
#include <errno.h>

#define MAX_EV 4096
static struct epoll_event g_events[MAX_EV];

CAMLprim value caml_epoll_create1(value unit) {
    int fd = epoll_create1(EPOLL_CLOEXEC);
    if (fd < 0) uerror("epoll_create1", Nothing);
    return Val_int(fd);
}

/* op: 1=ADD 2=DEL 3=MOD */
CAMLprim value caml_epoll_ctl(value epfd, value op, value fd, value events) {
    struct epoll_event ev;
    memset(&ev, 0, sizeof(ev));
    ev.events = (uint32_t) Int_val(events);
    ev.data.fd = Int_val(fd);
    int o = Int_val(op);
    int eop = o == 1 ? EPOLL_CTL_ADD : (o == 2 ? EPOLL_CTL_DEL : EPOLL_CTL_MOD);
    if (epoll_ctl(Int_val(epfd), eop, Int_val(fd), &ev) < 0)
        uerror("epoll_ctl", Nothing);
    return Val_unit;
}

/* Fill out_fds/out_evs (int arrays) with ready (fd,events); return count. */
CAMLprim value caml_epoll_wait(value epfd, value out_fds, value out_evs, value timeout_ms) {
    int max = Wosize_val(out_fds);
    if (max > MAX_EV) max = MAX_EV;
    int n;
    int ep = Int_val(epfd);
    int to = Int_val(timeout_ms);
    caml_release_runtime_system();
    n = epoll_wait(ep, g_events, max, to);
    caml_acquire_runtime_system();
    if (n < 0) {
        if (errno == EINTR) return Val_int(0);
        uerror("epoll_wait", Nothing);
    }
    for (int i = 0; i < n; i++) {
        Field(out_fds, i) = Val_int(g_events[i].data.fd);
        Field(out_evs, i) = Val_int((int) g_events[i].events);
    }
    return Val_int(n);
}

/* Receive one fd over a connected SOCK_SEQPACKET unix socket via SCM_RIGHTS.
   Returns the received fd (>=0), -1 on EAGAIN / no fd in the message, or
   -2 on EOF / error (caller should drop the control connection). */
CAMLprim value caml_recv_fd(value uds_fd) {
    struct msghdr mh;
    memset(&mh, 0, sizeof(mh));
    char dummy[1];
    struct iovec iov;
    iov.iov_base = dummy;
    iov.iov_len = sizeof(dummy);
    union {
        struct cmsghdr align;
        char buf[CMSG_SPACE(sizeof(int))];
    } ctrl;
    mh.msg_iov = &iov;
    mh.msg_iovlen = 1;
    mh.msg_control = ctrl.buf;
    mh.msg_controllen = sizeof(ctrl.buf);

    ssize_t n;
    do {
        n = recvmsg(Int_val(uds_fd), &mh, MSG_DONTWAIT | MSG_CMSG_CLOEXEC);
    } while (n < 0 && errno == EINTR);

    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) return Val_int(-1);
        return Val_int(-2);
    }
    if (n == 0) return Val_int(-2);

    struct cmsghdr *c = CMSG_FIRSTHDR(&mh);
    if (!c || c->cmsg_level != SOL_SOCKET || c->cmsg_type != SCM_RIGHTS) return Val_int(-1);
    int fd;
    memcpy(&fd, CMSG_DATA(c), sizeof(int));
    return Val_int(fd);
}
