ext except {
  class socket_except () {
    class optype (msg) {}
    class opvalue (msg) {}
    class eof (msg) {}
  }
}

var socket_excepts = excepts.socket_except ();

final class __socket_package () {
  var socket_excepts = excepts.socket_except ();

  extern _socket_errno, _socket_eof,
    _gethostinfo (), _setservent (), _getservent (),
    _endservent (), _socket_init ();
  private _socket_errno, _socket_eof,
    _gethostinfo, _setservent, _getservent,
    _endservent, _socket_init, host_info, serv_info;

  func generate_socket_exception (errno) {
    if (_socket_errno == _socket_eof)
      throw socket_excepts.eof ();
  }

  // If you change it, change code of _gethostinfo too.
  class host_info (final name, final aliases, final ipaddrs) {}

  func gethostinfo (str) {
    if (str == nil) str = "";
    else if (type (str) != vector || eltype (str) != char)
      throw socket_excepts.optype ();
    var h = host_info  ();
    h = _gethostinfo (str, h);
    // ??? socket_errno
    return h;
  }

  // If you change it, change code of _getservent too.
  class serv_info (final name, final aliases, final port, final proto) {}

  func getservices () {
    var s, v = [];

    _setservent();
    for (;;) {
      s = serv_info  ();
      s = _getservent (s);
      if (s == nil)
        break;
      ins (v, s, -1);
    }
    _endservent ();
    // ??? socket_errno
    return v;
  }

  func getservbyport (port, proto) {
    var s, t;

    if (type (proto) != vector || eltype (proto) != char)
      throw socket_excepts.optype ();
    if (port == nil)
      t = {};
    else if (type (port) != int)
      throw socket_excepts.optype ();
    _setservent();
    for (;;) {
      s = serv_info  ();
      s = _getservent (s);
      if (s == nil)
        break;
      if (port == s.port && s.proto == proto)
        break;
      else if (port == nil && s.proto == proto)
        t {s.port} = s;
    }
    _endservent ();
    // ??? socket_errno
    return (port == nil ? t : s);
  }

  func getservbyname (name, proto) {
    var s, t;

    if (type (proto) != vector || eltype (proto) != char)
      throw socket_excepts.optype ();
    if (name == nil)
      t = {};
    else if (type (name) != vector || eltype (name) != char)
      throw socket_excepts.optype ();
    _setservent();
    for (;;) {
      s = serv_info  ();
      s = _getservent (s);
      if (s == nil)
        break;
      if (name == s.name && s.proto == proto)
        break;
      else if (name == nil && s.proto == proto)
        t {s.name} = s;
    }
    _endservent ();
    // ??? socket_errno
    return (name == nil ? t : s);
  }

  private generate_socket_exception;

  extern _sread (), _swrite (), _recvfrom (), _sendto (), _accept (),
    _stream_client (), _dgram_client (), _stream_server (), _dgram_server ();
  private _sread, _swrite, _recvfrom, _sendto, _accept,
    _stream_client, _dgram_client, _stream_server, _dgram_server;

  // If you change it, change code of _recvfrom too.
  class datagram (str, peer_addr, port) {}
  private datagram;

  var proxy_sfd; private proxy_sfd;

  class stream_client (peer_addr, port) {
    var sfd; private sfd;

    func read (len) {
      if (type (len) != int)
        throw socket_excepts.optype ();
      else if (len < 0)
        throw socket_excepts.opvalue ();
      var str = _sread (sfd, len);
      if (str == nil)
        generate_socket_exception (_socket_errno);
      return str;
    }
    func write (str) {
      var nb = _swrite (sfd, str);
      if (nb == nil)
        generate_socket_exception (_socket_errno);
      return nb;
    }
    if (type (peer_addr) != vector || eltype (peer_addr) != char
        || type (port) != int)
      throw socket_excepts.optype ();
    sfd = (proxy_sfd == nil ? _stream_client (peer_addr, port) : proxy_sfd);
    println ("***", sfd, " ", proxy_sfd, peer_addr, port);
    proxy_sfd = nil;
    if (sfd == nil)
      generate_socket_exception (_socket_errno);
  }

  class dgram_client () {
    var sfd; private sfd;

    func recvfrom (len) {
      if (type (len) != int)
        throw socket_excepts.optype ();
  	else if (len < 0)
        throw socket_excepts.opvalue ();
      var dg = _recvfrom (sfd, len, datagram ());
      if (dg == nil)
        generate_socket_exception (_socket_errno);
      return dg;
    }
    func sendto (str, peer_addr, port) {
      if (type (str) != vector || eltype (str) != char
	  || type (peer_addr) != vector || eltype (peer_addr) != char
          || type (port) != int)
        throw socket_excepts.optype ();
      else if (port < 0)
        throw socket_excepts.opvalue ();
      var nb = _sendto (sfd, str, peer_addr, port);
      if (nb == nil)
        generate_socket_exception (_socket_errno);
      return nb;
    }
    sfd = _dgram_client ();
    if (sfd == nil)
      generate_socket_exception (_socket_errno);
  }

  class stream_server (port, queue_len) { // bind
    var sfd; private sfd;

    func accept () {
      var v = _accept (sfd);
      if (v == nil)
        generate_socket_exception (_socket_errno);
      println ("+++", v);
      proxy_sfd = v [0];      
      return stream_client (v [1], v [2]);
    }
    if (type (port) != int)
      throw socket_excepts.optype ();
    if (type (queue_len) != int)
      throw socket_excepts.optype ();
    sfd = _stream_server (port, queue_len);
    if (sfd == nil)
      generate_socket_exception (_socket_errno);
  }

  class dgram_server (port) {
    var sfd; private sfd;

    func recvfrom (len) {
      if (type (len) != int)
        throw socket_excepts.optype ();
    	else if (len < 0)
        throw socket_excepts.opvalue ();
      var dg = _recvfrom (sfd, len, datagram ());
      if (dg == nil)
        generate_socket_exception (_socket_errno);
      return dg;
    }
    func sendto (str, peer_addr, port) {
      if (type (str) != vector || eltype (str) != char
	  || type (peer_addr) != vector || eltype (peer_addr) != char
          || type (port) != int)
        throw socket_excepts.optype ();
      else if (port < 0)
        throw socket_excepts.opvalue ();
      var nb = _sendto (sfd, str, peer_addr, port);
      if (nb == nil)
        generate_socket_exception (_socket_errno);
      return nb;
    }
    if (type (port) != int)
      throw socket_excepts.optype ();
    sfd = _dgram_server (port);
    if (sfd == nil)
      generate_socket_exception (_socket_errno);
  }

  _socket_init ();
}

var sockets = __socket_package ();
