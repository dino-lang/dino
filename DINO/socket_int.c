/* We made all check inside DINO code therefore we trust in correct
   operand types. */

#include "d_extern.h"
#include <assert.h>
#include <errno.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#ifdef WIN32

#include <windows.h>

#define WIN_EXPORT  __declspec(dllexport)

BOOL APIENTRY
DllMain (HANDLE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
{
  return TRUE;
}

#else
#define WIN_EXPORT
#endif

WIN_EXPORT val_t _socket_errno, _socket_eof;

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
	abort ();
    }
  addr_p = ip_addres_p (name);
  if (addr_p)
    {
      if (inet_aton (name, &addr) == 0)
	abort ();
    }
  else
    {
      he = gethostbyname (name);
      if (he == NULL)
	return NULL;
      memcpy (&addr.s_addr, he->h_addr, sizeof (addr.s_addr));
    }
  return &addr;
}

WIN_EXPORT val_t
_gethostinfo (int npars, val_t *vals)
{
  int i, addr_p;
  char *name, str [1000];
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
      ER_SET_MODE (res, ER_NM_nil);
      /* ??? abort ();
	 ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
	 ER_set_i ((ER_node_t) &_socket_errno, 1); */
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
  ER_set_vect (res, instance);
  return val;
}

WIN_EXPORT val_t
_setservent (int npars, val_t *vals)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 0);
  ER_SET_MODE (res, ER_NM_nil);
  setservent (0);
  return val;
}

WIN_EXPORT val_t
_getservent (int npars, val_t *vals)
{
  int i;
  ER_node_t instance, var, vect;
  struct servent *se;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_instance);
  se = getservent ();
  if (se == NULL)
    {
      ER_SET_MODE (res, ER_NM_nil);
      /* ??? abort ();
	 ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
	 ER_set_i ((ER_node_t) &_socket_errno, 1); */
      return val;
    }
  instance = ER_instance ((ER_node_t) vals);
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
  ER_set_vect (res, instance);
  return val;
}

WIN_EXPORT val_t
_endservent (int npars, val_t *vals)
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 0);
  ER_SET_MODE (res, ER_NM_nil);
  endservent ();
  return val;
}

WIN_EXPORT val_t
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
  len = read (sd, ER_pack_els (vect), len);
  if (len == 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      _socket_errno = _socket_eof;      
    }
  else if (len < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
      abort ();
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

WIN_EXPORT val_t
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
  len = write (sd, str, len);
  if (len < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, len);
    }
  return val;
}

WIN_EXPORT val_t
_recvfrom (int npars, val_t *vals)
{
  int sd, len;
  socklen_t from_len;
  ER_node_t var, vect, instance;
  struct sockaddr_in s_addr;
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
  len = recvfrom (sd, ER_pack_els (vect), len, 0, (struct sockaddr *) &s_addr,
		  &from_len);
  if (len < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
      abort ();
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
      ER_set_vect (var, create_string (inet_ntoa (s_addr.sin_addr)));
      /* port */
      var = INDEXED_VAL (var, 1);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, ntohs (s_addr.sin_port));
      ER_SET_MODE (res, ER_NM_instance);
      ER_set_instance (res, instance);
    }
  return val;
}

WIN_EXPORT val_t
_sendto (int npars, val_t *vals)
{
  int sd, len, port;
  char *str, *addr;
  struct sockaddr_in s_addr;
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
  s_addr.sin_family = AF_INET;
  s_addr.sin_port = htons (port);
  sin_addr_ptr = get_ip_address (addr);
  if (sin_addr_ptr == NULL)
    abort (); /* ??? */
  s_addr.sin_addr = *sin_addr_ptr;
  len = sendto (sd, str, len, 0, (struct sockaddr *)&s_addr,
		sizeof (struct sockaddr_in));
  if (len < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
      abort ();
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, len);
    }
  return val;
}

WIN_EXPORT val_t
_accept (int npars, val_t *vals)
{
  int sd, new_sd;
  socklen_t addr_len;
  struct sockaddr_in s_addr;
  ER_node_t vect, var;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  sd = ER_i ((ER_node_t) vals);
  new_sd = accept (sd, (struct sockaddr *)&s_addr, &addr_len);
  if (new_sd < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
      abort ();
    }
  else
    {
      vect = create_unpack_vector (3);
      var = INDEXED_VAL (ER_unpack_els (vect), 0);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, new_sd);
      var = INDEXED_VAL (ER_unpack_els (vect), 1);
      ER_SET_MODE (var, ER_NM_vect);
      ER_set_vect (var, create_string (inet_ntoa (s_addr.sin_addr)));
      var = INDEXED_VAL (ER_unpack_els (vect), 2);
      ER_SET_MODE (var, ER_NM_int);
      ER_set_i (var, ntohs (s_addr.sin_port));
      ER_SET_MODE (res, ER_NM_vect);
      ER_set_vect (res, vect);
    }
  return val;
}

WIN_EXPORT val_t
_stream_client (int npars, val_t *vals)
{
  char *addr;
  int sfd, port;
  struct sockaddr_in s_addr;
  struct in_addr *sin_addr_ptr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  sfd = socket (AF_INET, SOCK_STREAM, 0);
  if (sfd < 0)
    printf ("sfd < 0\n"), abort (); /* ??? */
  /* connect */
  addr = ER_pack_els (ER_vect ((ER_node_t) vals));
  port = ER_i ((ER_node_t) (vals + 1));
  assert (port >= 0);
  s_addr.sin_family = AF_INET;
  s_addr.sin_port = htons (port);
  sin_addr_ptr = get_ip_address (addr);
  if (sin_addr_ptr == NULL)
    printf ("sin_addr_ptr == NULL\n"), abort (); /* ??? */
  s_addr.sin_addr = *sin_addr_ptr;
  if (connect (sfd, &s_addr, sizeof (s_addr)) < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
      perror ("connect :"), abort ();
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, sfd);
    }
  return val;
}

WIN_EXPORT val_t
_dgram_client (int npars, val_t *vals)
{
  char *addr;
  int sfd, port;
  struct sockaddr_in s_addr;
  struct in_addr *sin_addr_ptr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_vect
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  sfd = socket (AF_INET, SOCK_DGRAM, 0);
  if (sfd < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
      abort ();
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, sfd);
    }
  return val;
}

WIN_EXPORT val_t
_stream_server (int npars, val_t *vals)
{
  char *addr;
  int sfd, port, queue_len;
  struct sockaddr_in s_addr;
  struct in_addr *sin_addr_ptr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 2 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int
	  && ER_NODE_MODE ((ER_node_t) (vals + 1)) == ER_NM_int);
  port = ER_i ((ER_node_t) vals);
  queue_len = ER_i ((ER_node_t) (vals + 1));
  assert (port >= 0); /* queue_len??? */
  sfd = socket (AF_INET, SOCK_STREAM, 0);
  if (sfd < 0)
    printf ("sfd < 0\n"), abort (); /* ??? */
  s_addr.sin_family = AF_INET;
  s_addr.sin_port = htons (port);
  s_addr.sin_addr.s_addr = INADDR_ANY;
  if (bind (sfd, (struct sockaddr *) &s_addr, sizeof (s_addr)) < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
      perror ("bind: "), abort ();
    }
  else
    {
      listen (sfd, queue_len); /* ??? */
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, sfd);
    }
  return val;
}

WIN_EXPORT val_t
_dgram_server (int npars, val_t *vals)
{
  char *addr;
  int sfd, port, queue_len;
  struct sockaddr_in s_addr;
  struct in_addr *sin_addr_ptr;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 1 && ER_NODE_MODE ((ER_node_t) vals) == ER_NM_int);
  port = ER_i ((ER_node_t) vals);
  assert (port >= 0);
  sfd = socket (AF_INET, SOCK_DGRAM, 0);
  if (sfd < 0)
    printf ("sfd < 0\n"), abort (); /* ??? */
  s_addr.sin_family = AF_INET;
  s_addr.sin_port = htons (port);
  s_addr.sin_addr.s_addr = INADDR_ANY;
  if (bind (sfd, (struct sockaddr *) &s_addr, sizeof (s_addr)) < 0)
    {
      ER_SET_MODE (res, ER_NM_nil);
      errno; /* ??? */
      perror ("bind: "), abort ();
    }
  else
    {
      ER_SET_MODE (res, ER_NM_int);
      ER_set_i (res, sfd);
    }
  return val;
}

WIN_EXPORT val_t
_socket_init ()
{
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  ER_SET_MODE ((ER_node_t) &_socket_errno, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_errno, 0);
  ER_SET_MODE ((ER_node_t) &_socket_eof, ER_NM_int);
  ER_set_i ((ER_node_t) &_socket_eof, 1);
  ER_SET_MODE (res, ER_NM_nil);
  return val;
}
