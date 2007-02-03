/*
   Copyright (C) 1997-2007 Vladimir Makarov.

   Written by Vladimir Makarov <vmakarov@users.sourceforge.net>

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

/* We made all check inside DINO code therefore we trust in correct
   operand types. */

#ifdef HAVE_CONFIG_H
#include "d_config.h"
#endif
#include "d_extern.h"
#include <assert.h>
#include <errno.h>
#include <string.h>

#ifdef HAVE_SYS_SOCKET_H
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#if !defined(__CYGWIN__)
#ifndef h_errno
extern int h_errno;
#endif
#endif

val_t _socket_errno, _socket_invalid_address,
  _socket_host_not_found, _socket_no_address, _socket_no_recovery,
  _socket_try_again, _socket_eof;

static char *
skip_triple (char *str)
{
  if (!isdigit (*str))
    return NULL;
  str++;
  if (!isdigit (*str))
    return str;
  str++;
  if (!isdigit (*str))
    return str;
  return str + 1;
}

/* ??? IPv6 */
static int
ip_addres_p (char *name)
{
  char *s;
  int addr_p = 0; /* FALSE */

  s = skip_triple (name);
  if (s != NULL && *s == '.')
    {
      s = skip_triple (s + 1);
      if (s != NULL && *s == '.')
	{
	  s = skip_triple (s + 1);
	  if (s != NULL && *s == '.')
	    {
	      s = skip_triple (s + 1);
	      addr_p = s != NULL && *s == '\0';
	    }
	}
    }
  return addr_p;
}

static struct in_addr *
get_ip_address (char *name)
{
  int addr_p;
  struct hostent *he;
  char str [1000];
  static struct in_addr addr;

  if (*name == '\0')
    {
      name = str;
      if (gethostname (str, sizeof (str)) < 0)
	return NULL;
    }
  addr_p = ip_addres_p (name);
  if (addr_p)
    {
#if defined(HAVE_inet_aton)
      if (inet_aton (name, &addr)) == 0)
#else
      if ((addr.s_addr = inet_addr (name)) == -1)
#endif
	{
	  _socket_errno = _socket_invalid_address;      
	  return NULL;
	}
    }
  else
    {
      he = gethostbyname (name);
      if (he == NULL)
	{
	  if (h_errno == HOST_NOT_FOUND)
	    _socket_errno = _socket_host_not_found;
	  else if (h_errno == NO_DATA)
	    _socket_errno = _socket_no_address;
	  else if (h_errno == NO_RECOVERY)
	    _socket_errno = _socket_no_recovery;
	  else if (h_errno == TRY_AGAIN)
	    _socket_errno = _socket_try_again;
	  return NULL;
	}
      memcpy (&addr.s_addr, he->h_addr, sizeof (addr.s_addr));
    }
  return &addr;
}

val_t
_gethostinfo (int npars, val_t *vals)
{
  int i;
  char *name;
  struct hostent *he;
  struct in_addr *addr;
  ER_node_t vect, var, instance;
  val_t val;
  ER_node_t res = (ER_node_t) &val;
  
  assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_instance);
  name = ER_pack_els (ER_vect ((ER_node_t) vals));
  addr = get_ip_address (name);
  if (addr == NULL)
    he = NULL;
  else
    he = gethostbyaddr ((char *) &addr->s_addr, sizeof (addr->s_addr),
			AF_INET);
  if (he == NULL)
    {
      if (h_errno == HOST_NOT_FOUND)
	_socket_errno = _socket_host_not_found;
      else if (h_errno == NO_DATA)
	_socket_errno = _socket_no_address;
      else if (h_errno == NO_RECOVERY)
        _socket_errno = _socket_no_recovery;
      else if (h_errno == TRY_AGAIN)
	_socket_errno = _socket_try_again;
      ER_SET_MODE (res, ER_NM_nil);
      return val;
    }
  instance = ER_instance ((ER_node_t) (vals + 1));
  /* hostname */
  var = ER_instance_vars (instance);
  vect = create_string (he->h_name);
  ER_SET_MODE (var, ER_NM_vect);
  ER_set_vect (var, vect);
  /* aliases */
  var = INDEXED_VAL (var, 1);
  for (i = 0; he->h_aliases [i] != NULL; i++)
    ;
  if (i == 0)
    vect = create_empty_vector ();
  else
    vect = create_pack_vector (i, ER_NM_vect);
  for (i = 0; he->h_aliases [i] != NULL; i++)
    ((ER_node_t *) ER_pack_els (vect)) [i] = create_string (he->h_aliases [i]);
  ER_SET_MODE (var, ER_NM_vect);
  ER_set_vect (var, vect);
  /* ipaddrs */
  var = INDEXED_VAL (var, 1);
  for (i = 0; he->h_addr_list [i] != NULL; i++)
    ;
  vect = (i == 0
	  ? create_empty_vector () : create_pack_vector (i, ER_NM_vect));
  for (i = 0; he->h_addr_list [i] != NULL; i++)
    {
      memcpy (&addr->s_addr, he->h_addr_list [i], he->h_length);
      ((ER_node_t *) ER_pack_els (vect)) [i]
	= create_string (inet_ntoa (*addr));
    }
  ER_SET_MODE (var, ER_NM_vect);
  ER_set_vect (var, vect);
  ER_SET_MODE (res, ER_NM_instance);
  ER_set_instance (res, instance);
  return val;
}

static val_t
form_servent (ER_node_t instance, struct servent *se)
{
  ER_node_t var, vect;
  int i;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  if (se == NULL)
    {
      ER_SET_MODE (res, ER_NM_nil);
      return val;
    }
  /* name */
  var = ER_instance_vars (instance);
  vect = create_string (se->s_name);
  ER_SET_MODE (var, ER_NM_vect);
  ER_set_vect (var, vect);
  /* aliases */
  var = INDEXED_VAL (var, 1);
  for (i = 0; se->s_aliases [i] != NULL; i++)
    ;
  vect = (i == 0
	  ? create_empty_vector () : create_pack_vector (i, ER_NM_vect));
  for (i = 0; se->s_aliases [i] != NULL; i++)
    ((ER_node_t *) ER_pack_els (vect)) [i] = create_string (se->s_aliases [i]);
  ER_SET_MODE (var, ER_NM_vect);
  ER_set_vect (var, vect);
  /* port */
  var = INDEXED_VAL (var, 1);
  ER_SET_MODE (var, ER_NM_int);
  ER_set_i (var, (int) ntohs ((unsigned short) se->s_port));
  /* protocol */
  var = INDEXED_VAL (var, 1);
  vect = create_string (se->s_proto);
  ER_SET_MODE (var, ER_NM_vect);
  ER_set_vect (var, vect);
  ER_SET_MODE (res, ER_NM_instance);
  ER_set_instance (res, instance);
  return val;
}

val_t
_getservbyport (int npars, val_t *vals)
{
  ER_node_t instance, var, vect;
  int port;
  char *proto;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance);
  instance = ER_instance ((ER_node_t) vals);
  var = ER_instance_vars (instance);
  /* port */
  var = INDEXED_VAL (var, 2);
  assert (ER_NODE_MODE (var) == ER_NM_int);
  port = ER_i (var);
  /* proto */
  var = INDEXED_VAL (var, 1);
  assert (ER_NODE_MODE (var) == ER_NM_vect);
  vect = ER_vect (var);
  assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect);
  proto = ER_pack_els (vect);
  ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_errno, 0);
  return form_servent (instance, getservbyport (port, proto));
}


val_t
_getservbyname (int npars, val_t *vals)
{
  ER_node_t instance, var, vect;
  char *name, *proto;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance);
  instance = ER_instance ((ER_node_t) vals);
  /* name */
  var = ER_instance_vars (instance);
  assert (ER_NODE_MODE (var) == ER_NM_vect);
  vect = ER_vect (var);
  assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect);
  name = ER_pack_els (vect);
  /* proto */
  var = INDEXED_VAL (var, 3);
  assert (ER_NODE_MODE (var) == ER_NM_vect);
  vect = ER_vect (var);
  assert (ER_NODE_MODE (vect) == ER_NM_heap_pack_vect);
  proto = ER_pack_els (vect);
  ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_errno, 0);
  return form_servent (instance, getservbyname (name, proto));
}


val_t
_sread (int npars, val_t *vals)
{
  int sd, len;
  ER_node_t vect;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  sd = ER_i ((ER_node_t) vals);
  len = ER_i ((ER_node_t) (vals + 1));
  assert (len >= 0);
  vect = create_pack_vector (len + 1, ER_NM_char);
  ER_set_els_number (vect, 0);
  len = recv (sd, ER_pack_els (vect), len, 0);
  if (len == 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      _socket_errno = _socket_eof;      
    }
  else if (len < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      ((char *) ER_pack_els (vect)) [len] = '\0';
      ER_set_els_number (vect, len);
      ER_SET_MODE (res, ER_NM_vect);
      ER_set_vect (res, vect);
    }
  return val;
}

val_t
_swrite (int npars, val_t *vals)
{
  int sd, len;
  char *str;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_vect);
  sd = ER_i ((ER_node_t) vals);
  str = ER_pack_els (ER_vect ((ER_node_t) (vals + 1)));
  len = ER_els_number (ER_vect ((ER_node_t) (vals + 1)));
  len = send (sd, str, len, 0);
  if (len < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, len);
    }
  return val;
}

val_t
_recvfrom (int npars, val_t *vals)
{
  int sd, len;
#ifdef hpux
  int from_len;
#else
  socklen_t from_len;
#endif
  ER_node_t var, vect, instance;
  struct sockaddr_in saddr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 3 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int
	  && ER_NODE_MODE ((ER_node_t) (vals + 2)) == ER_NM_instance);
  sd = ER_i ((ER_node_t) vals);
  len = ER_i ((ER_node_t) (vals + 1));
  assert (len >= 0);
  vect = create_pack_vector (len + 1, ER_NM_char);
  ER_set_els_number (vect, 0);
  instance = ER_instance ((ER_node_t) (vals + 2));
  from_len = sizeof (struct sockaddr_in);
  len = recvfrom (sd, ER_pack_els (vect), len, 0, (struct sockaddr *) &saddr,
		  &from_len);
  if (len < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      /* str */
      ((char *) ER_pack_els (vect)) [len] = '\0';
      ER_set_els_number (vect, len);
      var = ER_instance_vars (instance);
      ER_SET_MODE (var, ER_NM_vect);
      ER_set_vect (var, vect);
      /* peer_addr */
      var = INDEXED_VAL (var, 1);
      ER_SET_MODE (var, ER_NM_vect);
      ER_set_vect (var, create_string (inet_ntoa (saddr.sin_addr)));
      /* port */
      var = INDEXED_VAL (var, 1);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, ntohs (saddr.sin_port));
      ER_SET_MODE (res, ER_NM_instance);
      ER_set_instance (res, instance);
    }
  return val;
}

val_t
_sendto (int npars, val_t *vals)
{
  int sd, len, port;
  char *str, *addr;
  struct sockaddr_in saddr;
  struct in_addr *sin_addr_ptr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 4 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_vect
	  && ER_NODE_MODE ((ER_node_t) (vals + 2)) == ER_NM_vect
	  && ER_NODE_MODE ((ER_node_t) (vals + 3)) == ER_NM_int);
  sd = ER_i ((ER_node_t) vals);
  str = ER_pack_els (ER_vect ((ER_node_t) (vals + 1)));
  len = ER_els_number (ER_vect ((ER_node_t) (vals + 1)));
  addr = ER_pack_els (ER_vect ((ER_node_t) (vals + 2)));
  port = ER_i ((ER_node_t) (vals + 3));
  assert (port >= 0);
  saddr.sin_family = AF_INET;
  saddr.sin_port = htons (port);
  sin_addr_ptr = get_ip_address (addr);
  if (sin_addr_ptr == NULL)
    {
      ER_SET_MODE (res, ER_NM_nil);
      return val;
    }
  saddr.sin_addr = *sin_addr_ptr;
  len = sendto (sd, str, len, 0, (struct sockaddr *)&saddr,
		sizeof (struct sockaddr_in));
  if (len < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, len);
    }
  return val;
}

val_t
_accept (int npars, val_t *vals)
{
  int sd, new_sd;
#ifdef hpux
  int addr_len;
#else
  socklen_t addr_len;
#endif
  struct sockaddr_in saddr;
  ER_node_t vect, var;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  sd = ER_i ((ER_node_t) vals);
  new_sd = accept (sd, (struct sockaddr *)&saddr, &addr_len);
  if (new_sd < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      vect = create_unpack_vector (3);
      var = INDEXED_VAL (ER_unpack_els (vect), 0);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, new_sd);
      var = INDEXED_VAL (ER_unpack_els (vect), 1);
      ER_SET_MODE (var, ER_NM_vect);
      ER_set_vect (var, create_string (inet_ntoa (saddr.sin_addr)));
      var = INDEXED_VAL (ER_unpack_els (vect), 2);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, ntohs (saddr.sin_port));
      ER_SET_MODE (res, ER_NM_vect);
      ER_set_vect (res, vect);
    }
  return val;
}

val_t
_stream_client (int npars, val_t *vals)
{
  char *addr;
  int sfd, port;
  struct sockaddr_in saddr;
  struct in_addr *sin_addr_ptr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  sfd = socket (AF_INET, SOCK_STREAM, 0);
  if (sfd < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
      return val;
    }
  /* connect */
  addr = ER_pack_els (ER_vect ((ER_node_t) vals));
  port = ER_i ((ER_node_t) (vals + 1));
  assert (port >= 0);
  saddr.sin_family = AF_INET;
  saddr.sin_port = htons (port);
  sin_addr_ptr = get_ip_address (addr);
  if (sin_addr_ptr == NULL)
    {
      ER_SET_MODE (res, ER_NM_nil);
      return val;
    }
  saddr.sin_addr = *sin_addr_ptr;
  if (connect (sfd, (struct sockaddr *) &saddr, sizeof (saddr)) != 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, sfd);
    }
  return val;
}

val_t
_dgram_client (int npars, val_t *vals)
{
  int sfd;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  sfd = socket (AF_INET, SOCK_DGRAM, 0);
  if (sfd < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, sfd);
    }
  return val;
}

val_t
_stream_server (int npars, val_t *vals)
{
  int sfd, port, queue_len;
  struct sockaddr_in saddr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  port = ER_i ((ER_node_t) vals);
  queue_len = ER_i ((ER_node_t) (vals + 1));
  assert (port >= 0);
  sfd = socket (AF_INET, SOCK_STREAM, 0);
  if (sfd < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
      return val;
    }
  saddr.sin_family = AF_INET;
  saddr.sin_port = htons (port);
  saddr.sin_addr.s_addr = INADDR_ANY;
  if (bind (sfd, (struct sockaddr *) &saddr, sizeof (saddr)) != 0
      || listen (sfd, queue_len) != 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, sfd);
    }
  return val;
}

val_t
_dgram_server (int npars, val_t *vals)
{
  int sfd, port;
  struct sockaddr_in saddr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  port = ER_i ((ER_node_t) vals);
  assert (port >= 0);
  sfd = socket (AF_INET, SOCK_DGRAM, 0);
  if (sfd < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
      return val;
    }
  saddr.sin_family = AF_INET;
  saddr.sin_port = htons (port);
  saddr.sin_addr.s_addr = INADDR_ANY;
  if (bind (sfd, (struct sockaddr *) &saddr, sizeof (saddr)) != 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
      ER_set_i ((ER_node_t) &_socket_errno, errno);
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, sfd);
    }
  return val;
}

val_t
_close_socket (int npars, val_t *vals)
{
  int sd;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  sd = ER_i ((ER_node_t) vals);
  /* We just ignore the errors becuse our goal to free the descriptors
     in calling function destroy.  */
  close (sd);
  ER_SET_MODE (res, ER_NM_nil);
  return val;
}

val_t
_socket_init (int npars, val_t *vals)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 0);

  ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_errno, 0);
  ER_SET_MODE ((ER_node_t) &_socket_invalid_address, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_invalid_address, 99999990);
  ER_SET_MODE ((ER_node_t) &_socket_host_not_found, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_host_not_found, 99999991);
  ER_SET_MODE ((ER_node_t) &_socket_no_address, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_no_address, 99999992);
  ER_SET_MODE ((ER_node_t) &_socket_no_recovery, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_no_recovery, 99999993);
  ER_SET_MODE ((ER_node_t) &_socket_try_again, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_try_again, 99999994);
  ER_SET_MODE ((ER_node_t) &_socket_eof, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_eof, 99999995);
  ER_SET_MODE (res, ER_NM_nil);
  return val;
}

val_t
_socket_fin (int npars, val_t *vals)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 0);
  ER_SET_MODE (res, ER_NM_nil);
  return val;
}

#if !defined(HAVE_DLOPEN) || defined(NO_EXTERN_SHLIB)

/* Function for implementing externals with static libraries.  See all
   externals name in ieee.d. */
void *
socket_address (const char *name)
{
  if (strcmp (name, "_socket_errno") == 0)
    return &_socket_errno;
  else if (strcmp (name, "_socket_invalid_address") == 0)
    return &_socket_invalid_address;
  else if (strcmp (name, "_socket_host_not_found") == 0)
    return &_socket_host_not_found;
  else if (strcmp (name, "_socket_no_address") == 0)
    return &_socket_no_address;
  else if (strcmp (name, "_socket_no_recovery") == 0)
    return &_socket_no_recovery;
  else if (strcmp (name, "_socket_try_again") == 0)
    return &_socket_try_again;
  else if (strcmp (name, "_socket_eof") == 0)
    return &_socket_eof;
  else if (strcmp (name, "_gethostinfo") == 0)
    return _gethostinfo;
  else if (strcmp (name, "_getservbyport") == 0)
    return _getservbyport;
  else if (strcmp (name, "_getservbyname") == 0)
    return _getservbyname;
  else if (strcmp (name, "_sread") == 0)
    return _sread;
  else if (strcmp (name, "_swrite") == 0)
    return _swrite;
  else if (strcmp (name, "_recvfrom") == 0)
    return _recvfrom;
  else if (strcmp (name, "_sendto") == 0)
    return _sendto;
  else if (strcmp (name, "_accept") == 0)
    return _accept;
  else if (strcmp (name, "_stream_client") == 0)
    return _stream_client;
  else if (strcmp (name, "_dgram_client") == 0)
    return _dgram_client;
  else if (strcmp (name, "_stream_server") == 0)
    return _stream_server;
  else if (strcmp (name, "_dgram_server") == 0)
    return _dgram_server;
  else if (strcmp (name, "_close_socket") == 0)
    return _close_socket;
  else if (strcmp (name, "_socket_init") == 0)
    return _socket_init;
  else if (strcmp (name, "_socket_fin") == 0)
    return _socket_fin;
  else
    return NULL;
}
#endif
