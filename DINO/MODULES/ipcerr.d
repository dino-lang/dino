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

obj ipc_errs {
  class ipc_syserror {use sys.syserror;}
  class ipc_eaddrinuse_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eaddrnotavail_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eafnosupport_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_ealready_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_econnaborted_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_econnrefused_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_econnreset_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_edestaddrreq_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_ehostdown_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_ehostunreach_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_einprogress_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eisconn_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_emsgsize_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_enetdown_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_enetreset_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_enetunreach_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_enobufs_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_enoprotoopt_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_enosr_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_enotconn_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_enotsock_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eopnotsupp_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_epfnosupport_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eprotonosupport_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eprototype_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eremoterelease_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eshutdown_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_esocktnosupport_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_etimedout_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_etoomanyrefs_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_ewouldblock_syserror (msg = nil) {use ipc_syserror former msg;}
  class ipc_eos_specific_syserror (msg = nil) {use ipc_syserror former msg;}

  priv extern _eaddrinuse_no, _eaddrnotavail_no, _eafnosupport_no,
    _ealready_no, _econnaborted_no, _econnrefused_no, _econnreset_no,
    _edestaddrreq_no, _ehostdown_no, _ehostunreach_no, _einprogress_no,
    _eisconn_no, _emsgsize_no, _enetdown_no, _enetreset_no,
    _enetunreach_no, _enobufs_no, _enoprotoopt_no, _enosr_no,
    _enotconn_no, _enotsock_no, _eopnotsupp_no, _epfnosupport_no,
    _eprotonosupport_no, _eprototype_no, _eremoterelease_no,
    _eshutdown_no, _esocktnosupport_no, _etimedout_no, _etoomanyrefs_no,
    _ewouldblock_no, _eos_specific_nos;

  priv extern _eaddrinuse_msg, _eaddrnotavail_msg, _eafnosupport_msg,
    _ealready_msg, _econnaborted_msg, _econnrefused_msg, _econnreset_msg,
    _edestaddrreq_msg, _ehostdown_msg, _ehostunreach_msg, _einprogress_msg,
    _eisconn_msg, _emsgsize_msg, _enetdown_msg, _enetreset_msg,
    _enetunreach_msg, _enobufs_msg, _enoprotoopt_msg, _enosr_msg,
    _enotconn_msg, _enotsock_msg, _eopnotsupp_msg, _epfnosupport_msg,
    _eprotonosupport_msg, _eprototype_msg, _eremoterelease_msg,
    _eshutdown_msg, _esocktnosupport_msg, _etimedout_msg, _etoomanyrefs_msg,
    _ewouldblock_msg, _eos_specific_msgs;

  priv extern _ipc_err_init ();
  _ipc_err_init ();

  // table no -> ipc exception
  var n2e = tab [];

  if (_eaddrinuse_no > 0)
    n2e [_eaddrinuse_no] = ipc_eaddrinuse_syserror (_eaddrinuse_msg);
  if (_eaddrnotavail_no > 0)
    n2e [_eaddrnotavail_no] = ipc_eaddrnotavail_syserror (_eaddrnotavail_msg);
  if (_eafnosupport_no > 0)
    n2e [_eafnosupport_no] = ipc_eafnosupport_syserror (_eafnosupport_msg);
  if (_ealready_no > 0)
    n2e [_ealready_no] = ipc_ealready_syserror (_ealready_msg);
  if (_econnaborted_no > 0)
    n2e [_econnaborted_no] = ipc_econnaborted_syserror (_econnaborted_msg);
  if (_econnrefused_no > 0)
    n2e [_econnrefused_no] = ipc_econnrefused_syserror (_econnrefused_msg);
  if (_econnreset_no > 0)
    n2e [_econnreset_no] = ipc_econnreset_syserror (_econnreset_msg);
  if (_edestaddrreq_no > 0)
    n2e [_edestaddrreq_no] = ipc_edestaddrreq_syserror (_edestaddrreq_msg);
  if (_ehostdown_no > 0)
    n2e [_ehostdown_no] = ipc_ehostdown_syserror (_ehostdown_msg);
  if (_ehostunreach_no > 0)
    n2e [_ehostunreach_no] = ipc_ehostunreach_syserror (_ehostunreach_msg);
  if (_einprogress_no > 0)
    n2e [_einprogress_no] = ipc_einprogress_syserror (_einprogress_msg);
  if (_eisconn_no > 0)
    n2e [_eisconn_no] = ipc_eisconn_syserror (_eisconn_msg);
  if (_emsgsize_no > 0)
    n2e [_emsgsize_no] = ipc_emsgsize_syserror (_emsgsize_msg);
  if (_enetdown_no > 0)
    n2e [_enetdown_no] = ipc_enetdown_syserror (_enetdown_msg);
  if (_enetreset_no > 0)
    n2e [_enetreset_no] = ipc_enetreset_syserror (_enetreset_msg);
  if (_enetunreach_no > 0)
    n2e [_enetunreach_no] = ipc_enetunreach_syserror (_enetunreach_msg);
  if (_enobufs_no > 0)
    n2e [_enobufs_no] = ipc_enobufs_syserror (_enobufs_msg);
  if (_enoprotoopt_no > 0)
    n2e [_enoprotoopt_no] = ipc_enoprotoopt_syserror (_enoprotoopt_msg);
  if (_enosr_no > 0)
    n2e [_enosr_no] = ipc_enosr_syserror (_enosr_msg);
  if (_enotconn_no > 0)
    n2e [_enotconn_no] = ipc_enotconn_syserror (_enotconn_msg);
  if (_enotsock_no > 0)
    n2e [_enotsock_no] = ipc_enotsock_syserror (_enotsock_msg);
  if (_eopnotsupp_no > 0)
    n2e [_eopnotsupp_no] = ipc_eopnotsupp_syserror (_eopnotsupp_msg);
  if (_epfnosupport_no > 0)
    n2e [_epfnosupport_no] = ipc_epfnosupport_syserror (_epfnosupport_msg);
  if (_eprotonosupport_no > 0)
    n2e [_eprotonosupport_no]
      = ipc_eprotonosupport_syserror (_eprotonosupport_msg);
  if (_eprototype_no > 0)
    n2e [_eprototype_no] = ipc_eprototype_syserror (_eprototype_msg);
  if (_eremoterelease_no > 0)
    n2e [_eremoterelease_no] = ipc_eremoterelease_syserror (_eremoterelease_msg);
  if (_eshutdown_no > 0)
    n2e [_eshutdown_no] = ipc_eshutdown_syserror (_eshutdown_msg);
  if (_esocktnosupport_no > 0)
    n2e [_esocktnosupport_no]
      = ipc_esocktnosupport_syserror (_esocktnosupport_msg);
  if (_etimedout_no > 0)
    n2e [_etimedout_no] = ipc_etimedout_syserror (_etimedout_msg);
  if (_etoomanyrefs_no > 0)
    n2e [_etoomanyrefs_no] = ipc_etoomanyrefs_syserror (_etoomanyrefs_msg);
  if (_ewouldblock_no > 0)
    n2e [_ewouldblock_no] = ipc_ewouldblock_syserror (_ewouldblock_msg);

  var i;
  if (_eos_specific_nos != nil)
    for (i = 0; i < #_eos_specific_nos; i++)
      n2e [_eos_specific_nos [i]]
	 = ipc_eos_specific_syserror (_eos_specific_msgs [i]);
}
