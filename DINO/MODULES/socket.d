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

include "ipcerr";

obj sockets {
  class socket_except () {use except;}
  class socket_optype_except () {use socket_except;}
  class socket_opvalue_except () {use socket_except;}
  class socket_eof_except () {use socket_except;}

  class socket_error (msg = nil) {use error former msg;}
  class socket_invalid_address_error (msg = nil) {use socket_error former msg;}
  class socket_host_not_found_error (msg = nil) {use socket_error former msg;}
  class socket_no_address_error (msg = nil) {use socket_error former msg;}
  class socket_no_recovery_error (msg = nil) {use socket_error former msg;}

  priv extern _socket_errno, _socket_invalid_address, _socket_host_not_found,
    _socket_no_address, _socket_no_recovery, _socket_try_again, _socket_eof,
    _gethostinfo (), _getservbyport (), _getservbyname (),
    _socket_init (), _socket_fin ();

  priv fun generate_socket_exception () {
    if (_socket_errno <= 0) throw socket_eof_except ();
    else if (_socket_errno == _socket_eof) throw socket_eof_except ();
    else if (_socket_errno in ipc_errs.n2e) throw ipc_errs.n2e [_socket_errno];
    else if (_socket_errno == _socket_invalid_address)
      throw socket_invalid_address_error ();
    else if (_socket_errno == _socket_host_not_found)
      throw socket_host_not_found_error ("host is unknown");
    else if (_socket_errno == _socket_no_address)
      throw socket_no_address_error ("does not have an IP address");
    else if (_socket_errno == _socket_no_recovery)
      throw socket_no_recovery_error ("non-recoverable name server error");
    else sys.__process_errno__ ("generate_socket_exception");
  }

  // If you change it, change code of _gethostinfo too.
  class host_info (val name, val aliases, val ipaddrs) {}

  fun gethostinfo (str) {
    if (str == nil) str = "";
    else if (type (str) != vec || eltype (str) != char)
      throw socket_optype_except ();
    var h = host_info  (nil, nil, nil);
    h = _gethostinfo (str, h);
    if (h == nil)
      generate_socket_exception ();
    return h;
  }

  // If you change it, change code of _getservbyname, _getservbyport too.
  class serv_info (val name, val aliases, val port, val proto) {}

  fun getservbyport (port, proto) {
    var s;

    if (type (proto) != vec || eltype (proto) != char || type (port) != int)
      throw socket_optype_except ();
    s = serv_info  (nil, nil, port, proto);
    s = _getservbyport (s);
    if (s == nil && _socket_errno != 0)
      generate_socket_exception ();
    return s;
  }

  fun getservbyname (name, proto) {
    var s;

    if (type (proto) != vec || eltype (proto) != char
	|| type (name) != vec || eltype (name) != char)
      throw socket_optype_except ();
    s = serv_info  (name, nil, nil, proto);
    s = _getservbyname (s);
    if (s == nil && _socket_errno != 0)
      generate_socket_exception ();
    return s;
  }

  priv extern _dsread (), _dswrite (), _recvfrom (), _sendto (), _accept (),
    _stream_client (), _dgram_client (), _stream_server (), _dgram_server (),
    _close_socket ();

  // If you change it, change code of _recvfrom too.
  priv class datagram (str, peer_addr, port) {}

  priv var proxy_sfd = nil;

  class stream_client (peer_addr, port) {
    priv var sfd;

    fun read (len) {
      if (type (len) != int)
        throw socket_optype_except ();
      else if (len < 0)
        throw socket_opvalue_except ();
      var str = _dsread (sfd, len);
      if (str == nil)
        generate_socket_exception ();
      return str;
    }
    fun write (str) {
      var nb = _dswrite (sfd, str);
      if (nb == nil)
        generate_socket_exception ();
      return nb;
    }

    priv fun destroy () {if (sfd != nil) _close_socket (sfd);}

    if (type (peer_addr) != vec || eltype (peer_addr) != char
        || type (port) != int)
      throw socket_optype_except ();
    sfd = (proxy_sfd == nil ? _stream_client (peer_addr, port) : proxy_sfd);
    proxy_sfd = nil;
    if (sfd == nil)
      generate_socket_exception ();
  }

  class dgram_client () {
    priv var sfd;

    fun recvfrom (len) {
      if (type (len) != int)
        throw socket_optype_except ();
      else if (len < 0)
        throw socket_opvalue_except ();
      var dg = _recvfrom (sfd, len, datagram ());
      if (dg == nil)
        generate_socket_exception ();
      return dg;
    }
    fun sendto (str, peer_addr, port) {
      if (type (str) != vec || eltype (str) != char
	  || type (peer_addr) != vec || eltype (peer_addr) != char
          || type (port) != int)
        throw socket_optype_except ();
      else if (port < 0)
        throw socket_opvalue_except ();
      var nb = _sendto (sfd, str, peer_addr, port);
      if (nb == nil)
        generate_socket_exception ();
      return nb;
    }

    priv fun destroy () {if (sfd != nil) _close_socket (sfd);}

    sfd = _dgram_client ();
    if (sfd == nil)
      generate_socket_exception ();
  }

  class stream_server (port, queue_len) { // bind
    priv var sfd;

    fun accept () {
      var v = _accept (sfd);
      if (v == nil)
        generate_socket_exception ();
      proxy_sfd = v [0];      
      return stream_client (v [1], v [2]);
    }

    priv fun destroy () {if (sfd != nil) _close_socket (sfd);}

    if (type (port) != int)
      throw socket_optype_except ();
    if (type (queue_len) != int)
      throw socket_optype_except ();
    sfd = _stream_server (port, queue_len);
    if (sfd == nil)
      generate_socket_exception ();
  }

  class dgram_server (port) {
    priv var sfd;

    fun recvfrom (len) {
      if (type (len) != int)
        throw socket_optype_except ();
    	else if (len < 0)
        throw socket_opvalue_except ();
      var dg = _recvfrom (sfd, len, datagram ());
      if (dg == nil)
        generate_socket_exception ();
      return dg;
    }
    fun sendto (str, peer_addr, port) {
      if (type (str) != vec || eltype (str) != char
	  || type (peer_addr) != vec || eltype (peer_addr) != char
          || type (port) != int)
        throw socket_optype_except ();
      else if (port < 0)
        throw socket_opvalue_except ();
      var nb = _sendto (sfd, str, peer_addr, port);
      if (nb == nil)
        generate_socket_exception ();
      return nb;
    }

    priv fun destroy () {if (sfd != nil) _close_socket (sfd);}

    if (type (port) != int)
      throw socket_optype_except ();
    sfd = _dgram_server (port);
    if (sfd == nil)
      generate_socket_exception ();
  }

  priv fun destroy () {_socket_fin ();}

  _socket_init ();
}
