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

CAMLprim value stub_cstruct_write(value val_fd, value val_c)
{
  CAMLparam2(val_fd, val_c);
  CAMLlocal3(val_buf, val_ofs, val_len);
  val_buf = Field(val_c, 0);
  val_ofs = Field(val_c, 1);
  val_len = Field(val_c, 2);
  void *buf = (char *)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  size_t len = Long_val(val_len);
  ssize_t n = 0;

#ifdef _WIN32
  int win32err = 0;
  switch (Descr_kind_val(val_fd))
  {
  case KIND_SOCKET:
    SOCKET s = Socket_val(val_fd);

    caml_release_runtime_system();
    n = send(s, buf, len, 0);
    win32err = WSAGetLastError();
    caml_acquire_runtime_system();

    if (n == SOCKET_ERROR)
    {
      win32_maperr(win32err);
      unix_error(errno, "stub_cstruct_write", Nothing);
    }
    break;
  case KIND_HANDLE:
    HANDLE h = Handle_val(val_fd);
    DWORD numwritten;
    caml_release_runtime_system();
    int ok = WriteFile(h, buf, len, &numwritten, NULL);
    win32err = GetLastError();

    n = numwritten;
    caml_acquire_runtime_system();

    if (!ok)
    {
      win32_maperr(win32err);
      uerror("stub_cstruct_write", Nothing);
    }
    break;
  default:
    caml_failwith("unknown Descr_kind_val");
  }
#else
  caml_release_runtime_system();
  n = write(Int_val(val_fd), buf, len);
  caml_acquire_runtime_system();
  if (n < 0)
    uerror("stub_cstruct_write", Nothing);
#endif
  CAMLreturn(Val_int(n));
}
