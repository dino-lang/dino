/*
   Copyright (C) 1997-2016 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@gcc.gnu.org>

   This file is part of interpreter of DINO.

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

*/

#include "d_api.h"
#include <errno.h>

val_t _eaddrinuse_no, _eaddrnotavail_no, _eafnosupport_no,
  _ealready_no, _econnaborted_no, _econnrefused_no, _econnreset_no,
  _edestaddrreq_no, _ehostdown_no, _ehostunreach_no, _einprogress_no,
  _eisconn_no, _emsgsize_no, _enetdown_no, _enetreset_no,
  _enetunreach_no, _enobufs_no, _enoprotoopt_no, _enosr_no,
  _enotconn_no, _enotsock_no, _eopnotsupp_no, _epfnosupport_no,
  _eprotonosupport_no, _eprototype_no, _eremoterelease_no,
  _eshutdown_no, _esocktnosupport_no, _etimedout_no, _etoomanyrefs_no,
  _ewouldblock_no, _eos_specific_nos;

val_t _eaddrinuse_msg, _eaddrnotavail_msg, _eafnosupport_msg,
  _ealready_msg, _econnaborted_msg, _econnrefused_msg, _econnreset_msg,
  _edestaddrreq_msg, _ehostdown_msg, _ehostunreach_msg, _einprogress_msg,
  _eisconn_msg, _emsgsize_msg, _enetdown_msg, _enetreset_msg,
  _enetunreach_msg, _enobufs_msg, _enoprotoopt_msg, _enosr_msg,
  _enotconn_msg, _enotsock_msg, _eopnotsupp_msg, _epfnosupport_msg,
  _eprotonosupport_msg, _eprototype_msg, _eremoterelease_msg,
  _eshutdown_msg, _esocktnosupport_msg, _etimedout_msg, _etoomanyrefs_msg,
  _ewouldblock_msg, _eos_specific_msgs;

static void
set_ipc_err_vars (int no, val_t *val_no, val_t *val_msg, const char *msg)
{
  ER_node_t vect;

  ER_SET_MODE ((ER_node_t) val_no, ER_NM_int);
  ER_set_i ((ER_node_t) val_no, no);
  vect = create_string (msg);
  ER_SET_MODE ((ER_node_t) val_msg, ER_NM_vect);
  set_vect_dim ((ER_node_t) val_msg, vect, 0);
}

val_t
_ipc_err_init (int npars, val_t *vals)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  d_assert (npars == 0);

#if defined(EADDRINUSE)
  set_ipc_err_vars (EADDRINUSE, &_eaddrinuse_no, &_eaddrinuse_msg,
		    "address already in use");
#else
  set_ipc_err_vars (-1, &_eaddrinuse_no, &_eaddrinuse_msg, "");
#endif
  
#if defined(EADDRNOTAVAIL)
  set_ipc_err_vars (EADDRNOTAVAIL, &_eaddrnotavail_no, &_eaddrnotavail_msg,
		    "can't assign requested address");
#else
  set_ipc_err_vars (-1, &_eaddrnotavail_no, &_eaddrnotavail_msg, "");
#endif

#if defined(EAFNOSUPPORT)
  set_ipc_err_vars (EAFNOSUPPORT, &_eafnosupport_no, &_eafnosupport_msg,
		    "address family not supported by protocol family");
#else
  set_ipc_err_vars (-1, &_eafnosupport_no, &_eafnosupport_msg, "");
#endif

#if defined(EALREADY)
  set_ipc_err_vars (EALREADY, &_ealready_no, &_ealready_msg,
		    "operation already in progress");
#else
  set_ipc_err_vars (-1, &_ealready_no, &_ealready_msg, "");
#endif

#if defined(ECONNABORTED)
  set_ipc_err_vars (ECONNABORTED, &_econnaborted_no, &_econnaborted_msg,
		    "software caused connection abort");
#else
  set_ipc_err_vars (-1, &_econnaborted_no, &_econnaborted_msg, "");
#endif

#if defined(ECONNREFUSED)
  set_ipc_err_vars (ECONNREFUSED, &_econnrefused_no, &_econnrefused_msg,
		    "connection refused");
#else
  set_ipc_err_vars (-1, &_econnrefused_no, &_econnrefused_msg, "");
#endif

#if defined(ECONNRESET)
  set_ipc_err_vars (ECONNRESET, &_econnreset_no, &_econnreset_msg,
		    "connection reset by peer");
#else
  set_ipc_err_vars (-1, &_econnreset_no, &_econnreset_msg, "");
#endif

#if defined(EDESTADDRREQ)
  set_ipc_err_vars (EDESTADDRREQ, &_edestaddrreq_no, &_edestaddrreq_msg,
		    "destination address required");
#else
  set_ipc_err_vars (-1, &_edestaddrreq_no, &_edestaddrreq_msg, "");
#endif

#if defined(EHOSTDOWN)
  set_ipc_err_vars (EHOSTDOWN, &_ehostdown_no, &_ehostdown_msg,
		    "host is down");
#else
  set_ipc_err_vars (-1, &_ehostdown_no, &_ehostdown_msg, "n");
#endif

#if defined(EHOSTUNREACH)
  set_ipc_err_vars (EHOSTUNREACH, &_ehostunreach_no, &_ehostunreach_msg,
		    "no route to host");
#else
  set_ipc_err_vars (-1, &_ehostunreach_no, &_ehostunreach_msg, "");
#endif

#if defined(EINPROGRESS)
  set_ipc_err_vars (EINPROGRESS, &_einprogress_no, &_einprogress_msg,
		    "operation now in progress");
#else
  set_ipc_err_vars (-1, &_einprogress_no, &_einprogress_msg, "");
#endif

#if defined(EISCONN)
  set_ipc_err_vars (EISCONN, &_eisconn_no, &_eisconn_msg,
		    "socket is already connected");
#else
  set_ipc_err_vars (-1, &_eisconn_no, &_eisconn_msg, "");
#endif

#if defined(EMSGSIZE)
  set_ipc_err_vars (EMSGSIZE, &_emsgsize_no, &_emsgsize_msg,
		    "message too long");
#else
  set_ipc_err_vars (-1, &_emsgsize_no, &_emsgsize_msg, "");
#endif

#if defined(ENETDOWN)
  set_ipc_err_vars (ENETDOWN, &_enetdown_no, &_enetdown_msg,
		    "network is down");
#else
  set_ipc_err_vars (-1, &_enetdown_no, &_enetdown_msg, "");
#endif

#if defined(ENETRESET)
  set_ipc_err_vars (ENETRESET, &_enetreset_no, &_enetreset_msg,
		    "network dropped connection because of reset");
#else
  set_ipc_err_vars (-1, &_enetreset_no, &_enetreset_msg, "");
#endif

#if defined(ENETUNREACH)
  set_ipc_err_vars (ENETUNREACH, &_enetunreach_no, &_enetunreach_msg,
		    "network is unreachable");
#else
  set_ipc_err_vars (-1, &_enetunreach_no, &_enetunreach_msg, "");
#endif

#if defined(ENOBUFS)
  set_ipc_err_vars (ENOBUFS, &_enobufs_no, &_enobufs_msg,
		    "no buffer space available");
#else
  set_ipc_err_vars (-1, &_enobufs_no, &_enobufs_msg, "");
#endif

#if defined(ENOPROTOOPT)
  set_ipc_err_vars (ENOPROTOOPT, &_enoprotoopt_no, &_enoprotoopt_msg,
		    "protocol not available");
#else
  set_ipc_err_vars (-1, &_enoprotoopt_no, &_enoprotoopt_msg, "");
#endif

#ifdef ENOSR
  set_ipc_err_vars (ENOSR, &_enosr_no, &_enosr_msg,
		    "out of streams resources");
#else
  set_ipc_err_vars (-1, &_enosr_no, &_enosr_msg, "");
#endif

#if defined(ENOTCONN)
  set_ipc_err_vars (ENOTCONN, &_enotconn_no, &_enotconn_msg,
		    "socket is not connected");
#else
  set_ipc_err_vars (-1, &_enotconn_no, &_enotconn_msg, "");
#endif

#if defined(ENOTSOCK)
  set_ipc_err_vars (ENOTSOCK, &_enotsock_no, &_enotsock_msg,
		    "socket operation on non-socket");
#else
  set_ipc_err_vars (-1, &_enotsock_no, &_enotsock_msg, "");
#endif

#if defined(EOPNOTSUPP)
  set_ipc_err_vars (EOPNOTSUPP, &_eopnotsupp_no, &_eopnotsupp_msg,
		    "operation not supported on socket");
#else
  set_ipc_err_vars (-1, &_eopnotsupp_no, &_eopnotsupp_msg, "");
#endif

#if defined(EPFNOSUPPORT)
  set_ipc_err_vars (EPFNOSUPPORT, &_epfnosupport_no, &_epfnosupport_msg,
		    "protocol family not supported");
#else
  set_ipc_err_vars (-1, &_epfnosupport_no, &_epfnosupport_msg, "");
#endif

#if defined(EPROTONOSUPPORT)
  set_ipc_err_vars (EPROTONOSUPPORT, &_eprotonosupport_no,
		    &_eprotonosupport_msg, "protocol not supported");
#else
  set_ipc_err_vars (-1, &_eprotonosupport_no, &_eprotonosupport_msg, "");
#endif

#if defined(EPROTOTYPE)
  set_ipc_err_vars (EPROTOTYPE, &_eprototype_no, &_eprototype_msg,
		    "protocol wrong type for socket");
#else
  set_ipc_err_vars (-1, &_eprototype_no, &_eprototype_msg, "");
#endif

#ifdef EREMOTERELEASE
  set_ipc_err_vars (EREMOTERELEASE, &_eremoterelease_no, &_eremoterelease_msg,
		    "remote peer released connection");
#else
  set_ipc_err_vars (-1, &_eremoterelease_no, &_eremoterelease_msg, "");
#endif

#if defined(ESHUTDOWN)
  set_ipc_err_vars (ESHUTDOWN, &_eshutdown_no, &_eshutdown_msg,
		    "can't send after socket shutdown");
#else
  set_ipc_err_vars (-1, &_eshutdown_no, &_eshutdown_msg, "");
#endif

#if defined(ESOCKTNOSUPPORT)
  set_ipc_err_vars (ESOCKTNOSUPPORT, &_esocktnosupport_no,
		    &_esocktnosupport_msg, "socket type not supported");
#else
  set_ipc_err_vars (-1, &_esocktnosupport_no, &_esocktnosupport_msg, "");
#endif

#if defined(ETIMEDOUT)
  set_ipc_err_vars (ETIMEDOUT, &_etimedout_no, &_etimedout_msg,
		    "connection timed out");
#else
  set_ipc_err_vars (-1, &_etimedout_no, &_etimedout_msg, "");
#endif

#ifdef ETOOMANYREFS
  set_ipc_err_vars (ETOOMANYREFS, &_etoomanyrefs_no, &_etoomanyrefs_msg,
		    "too many references: can't splice");
#else
  set_ipc_err_vars (-1, &_etoomanyrefs_no, &_etoomanyrefs_msg, "");
#endif

#if defined(EWOULDBLOCK)
#if defined(EAGAIN) && EAGAIN != EWOULDBLOCK
  set_ipc_err_vars (EWOULDBLOCK, &_ewouldblock_no, &_ewouldblock_msg,
		    "operation would block");
#else
  set_ipc_err_vars (-1, &_ewouldblock_no, &_ewouldblock_msg, "");
#endif
  set_ipc_err_vars (-1, &_ewouldblock_no, &_ewouldblock_msg, "");
#endif

  ER_SET_MODE ((ER_node_t) &_eos_specific_nos, ER_NM_nil);
  ER_SET_MODE ((ER_node_t) &_eos_specific_msgs, ER_NM_nil);

  ER_SET_MODE (res, ER_NM_nil);
  return val;
}
