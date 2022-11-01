#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/threads.h>

#include <stdio.h>

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <ws2tcpip.h>
#include <NTSecAPI.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#endif

CAMLprim value stub_cstruct_recv(value val_fd, value val_c)
{
    CAMLparam2(val_fd, val_c);
    CAMLlocal3(val_buf, val_ofs, val_len);

    val_buf = Field(val_c, 0);
    val_ofs = Field(val_c, 1);
    val_len = Field(val_c, 2);

    void *buf = (void *)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
    size_t len = (size_t)Long_val(val_len);
    int n = 0;
#ifdef WIN32
    int win32err = 0;
    if (Descr_kind_val(val_fd) != KIND_SOCKET)
        unix_error(EINVAL, "stub_cstruct_recv", Nothing);

    SOCKET s = Socket_val(val_fd);

    caml_release_runtime_system();
    n = recv(s, buf, len, 0);
    win32err = WSAGetLastError();
    caml_acquire_runtime_system();

    if (n == SOCKET_ERROR)
    {
        win32_maperr(win32err);
        unix_error(errno, "recv", Nothing);
    }
#else
    int fd = Int_val(val_fd);

    caml_release_runtime_system();
    n = recv(fd, buf, len, 0);
    caml_acquire_runtime_system();

    if (n < 0)
        unix_error(errno, "recv", Nothing);
#endif
    CAMLreturn(Val_int(n));
}
