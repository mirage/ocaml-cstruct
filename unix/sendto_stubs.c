#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/socketaddr.h>

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#else
#include <sys/socket.h>
#endif

static int msg_flag_table[] = { /* XXX */
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

CAMLprim value stub_cstruct_sendto(value val_fd, value val_c, value val_flags, value val_daddr)
{
  CAMLparam4(val_fd, val_c, val_flags, val_daddr);
  CAMLlocal5(val_buf, val_ofs, val_len, val_addr, val_res);
  union sock_addr_union addr;
  socklen_param_type addr_len;
  uint8_t *buf;
  size_t len;
  ssize_t n;
  int cv_flags;
#ifdef WIN32
  int win32err = 0;
  SOCKET s = Socket_val(val_fd);
#endif

  val_buf = Field(val_c, 0);
  val_ofs = Field(val_c, 1);
  val_len = Field(val_c, 2);

  buf = (uint8_t *)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  len = Long_val(val_len);
  get_sockaddr(val_daddr, &addr, &addr_len);
  cv_flags = caml_convert_flag_list(val_flags, msg_flag_table);

#ifdef WIN32
  if (Descr_kind_val(val_fd) != KIND_SOCKET)
      unix_error(EINVAL, "stub_cstruct_sendto", Nothing);

  caml_release_runtime_system();
  n = sendto(s, buf, len, cv_flags, &addr.s_gen, addr_len);
  win32err = WSAGetLastError();
  caml_acquire_runtime_system();

  if (n == SOCKET_ERROR)
  {
      win32_maperr(win32err);
      uerror("stub_cstruct_sendto", Nothing);
  }
#else
  caml_release_runtime_system();
  n = sendto(Int_val(val_fd), buf, len, cv_flags, &addr.s_gen, addr_len);
  caml_acquire_runtime_system();

  if (n == -1)
    uerror("stub_cstruct_sendto", Nothing);
#endif
  CAMLreturn (Val_int(n));
}
