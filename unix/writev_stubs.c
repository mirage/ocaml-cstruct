#define _XOPEN_SOURCE /* IOV_MAX */
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
#ifndef _WIN32
#include <sys/uio.h>
#include <limits.h>
#endif

CAMLprim
    value
    stub_cstruct_iov_max(value unit)
{
  CAMLparam1(unit);
#ifdef IOV_MAX
  CAMLreturn(Val_int(IOV_MAX));
#else
  CAMLreturn(Val_int(0));
#endif
}

CAMLprim value stub_cstruct_writev(value fd, value val_list)
{
  CAMLparam2(fd, val_list);
  CAMLlocal5(next, head, val_buf, val_ofs, val_len);
  int i;
#ifdef _WIN32
  caml_failwith("writev is not supported on Win32");
#else
  struct iovec iovec[IOV_MAX];
  int c_fd = Int_val(fd);

  next = val_list;
  /* Calculate the length of the val_list */
  int length = 0;
  for (next = val_list; next != Val_emptylist; next = Field(next, 1))
    length++;
  /* Only copy up to the iovec array size */
  if (length > IOV_MAX)
    length = IOV_MAX;
  next = val_list;
  for (i = 0; i < length; i++)
  {
    head = Field(next, 0);
    val_buf = Field(head, 0);
    val_ofs = Field(head, 1);
    val_len = Field(head, 2);
    iovec[i].iov_base = (char *)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
    iovec[i].iov_len = Long_val(val_len);
    next = Field(next, 1);
  }

  caml_release_runtime_system();
  ssize_t n = writev(c_fd, iovec, length);
  caml_acquire_runtime_system();
  if (n < 0)
    uerror("stub_cstruct_writev", Nothing);
  CAMLreturn(Val_int(n));
#endif
}
