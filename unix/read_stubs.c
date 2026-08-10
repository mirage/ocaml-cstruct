#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/threads.h>

#include <stdio.h>
#include <errno.h>

CAMLprim value stub_cstruct_read(value val_fd, value val_c)
{
  CAMLparam2(val_fd, val_c);
  CAMLlocal3(val_buf, val_ofs, val_len);
  uint8_t *buf;
  size_t len;
  ssize_t n = 0;
#ifdef WIN32
  int win32err = 0;
  SOCKET s;
  HANDLE h;
  DWORD numread;
  int ok;
#endif
  val_buf = Field(val_c, 0);
  val_ofs = Field(val_c, 1);
  val_len = Field(val_c, 2);

  buf = (uint8_t *)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  len = (size_t)Long_val(val_len);

#ifdef WIN32
  switch (Descr_kind_val(val_fd))
  {
  case KIND_SOCKET:
    s = Socket_val(val_fd);

    caml_release_runtime_system();
    n = recv(s, buf, len, 0);
    win32err = WSAGetLastError();
    caml_acquire_runtime_system();

    if (n == SOCKET_ERROR)
    {
      win32_maperr(win32err);
      uerror("stub_cstruct_read", Nothing);
    }
    break;
  case KIND_HANDLE:
    h = Handle_val(val_fd);
    caml_release_runtime_system();
    ok = ReadFile(h, buf, len, &numread, NULL);
    win32err = GetLastError();
    n = numread;
    caml_acquire_runtime_system();

    if (!ok)
    {
      win32_maperr(win32err);
      uerror("stub_cstruct_read", Nothing);
    }
    break;
  default:
    caml_failwith("unknown Descr_kind_val");
  }
#else
  caml_release_runtime_system();
  n = read(Int_val(val_fd), buf, len);
  caml_acquire_runtime_system();
  if (n < 0)
    uerror("stub_cstruct_read", Nothing);
#endif
  CAMLreturn(Val_int(n));
}
