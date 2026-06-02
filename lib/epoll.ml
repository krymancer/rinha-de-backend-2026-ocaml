(* Thin epoll wrapper. fds are raw ints (Unix.file_descr is an int on Unix). *)

external create1 : unit -> int = "caml_epoll_create1"
external ctl : int -> int -> int -> int -> unit = "caml_epoll_ctl"
external wait : int -> int array -> int array -> int -> int = "caml_epoll_wait"

(* Like [wait] but busy-polls with epoll_wait(0) for up to [spin_us] µs (holding
   the runtime lock, keeping the core in C0) before falling back to a blocking
   wait. [spin_us]=0 behaves exactly like [wait epfd _ _ (-1)]. *)
external wait_spin : int -> int array -> int array -> int -> int = "caml_epoll_wait_spin"

(* receive a passed fd over a SOCK_SEQPACKET unix socket; >=0 fd, -1 EAGAIN, -2 EOF/err *)
external recv_fd : int -> int = "caml_recv_fd"

external int_of_fd : Unix.file_descr -> int = "%identity"
external fd_of_int : int -> Unix.file_descr = "%identity"

(* event flags *)
let in_ = 0x001
let out = 0x004
let err = 0x008
let hup = 0x010
let rdhup = 0x2000

(* ctl ops *)
let op_add = 1
let op_del = 2
let op_mod = 3

let add epfd fd events = ctl epfd op_add fd events
let modify epfd fd events = ctl epfd op_mod fd events
let del epfd fd = ctl epfd op_del fd 0
