#include "d_config.h"
#include "d_extern.h"
#include <assert.h>
#include <errno.h>

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

WIN_EXPORT val_t _eaddrinuse_no, _eaddrnotavail_no, _eafnosupport_no,
  _ealready_no, _econnaborted_no, _econnrefused_no, _econnreset_no,
  _edestaddrreq_no, _ehostdown_no, _ehostunreach_no, _einprogress_no,
  _eisconn_no, _emsgsize_no, _enetdown_no, _enetreset_no,
  _enetunreach_no, _enobufs_no, _enoprotoopt_no, _enosr_no,
  _enotconn_no, _enotsock_no, _eopnotsupp_no, _epfnosupport_no,
  _eprotonosupport_no, _eprototype_no, _eremoterelease_no,
  _eshutdown_no, _esocktnosupport_no, _etimedout_no, _etoomanyrefs_no,
  _ewouldblock_no;

WIN_EXPORT val_t _eaddrinuse_msg, _eaddrnotavail_msg, _eafnosupport_msg,
  _ealready_msg, _econnaborted_msg, _econnrefused_msg, _econnreset_msg,
  _edestaddrreq_msg, _ehostdown_msg, _ehostunreach_msg, _einprogress_msg,
  _eisconn_msg, _emsgsize_msg, _enetdown_msg, _enetreset_msg,
  _enetunreach_msg, _enobufs_msg, _enoprotoopt_msg, _enosr_msg,
  _enotconn_msg, _enotsock_msg, _eopnotsupp_msg, _epfnosupport_msg,
  _eprotonosupport_msg, _eprototype_msg, _eremoterelease_msg,
  _eshutdown_msg, _esocktnosupport_msg, _etimedout_msg, _etoomanyrefs_msg,
  _ewouldblock_msg;

static void
set_ipc_err_vars (int no, val_t *val_no, val_t *val_msg, const char *msg)
{
  ER_node_t vect;

  ER_SET_MODE ((ER_node_t) val_no, ER_NM_int);
  ER_set_i ((ER_node_t) val_no, no);
  vect = create_string (msg);
  ER_SET_MODE ((ER_node_t) val_msg, ER_NM_vect);
  ER_set_vect ((ER_node_t) val_msg, vect);
}

WIN_EXPORT val_t
_ipc_err_init (int npars, val_t *vals)
{
  assert (npars == 0);
#ifdef EADDRINUSE
  set_ipc_err_vars (EADDRINUSE, &_eaddrinuse_no, &_eaddrinuse_msg,
		    "address already in use");
#else
  set_ipc_err_vars (-1, &_eaddrinuse_no, &_eaddrinuse_msg, "");
#endif
#ifdef EADDRNOTAVAIL
  set_ipc_err_vars (EADDRNOTAVAIL, &_eaddrnotavail_no, &_eaddrnotavail_msg,
		    "can't assign requested address");
#else
  set_ipc_err_vars (-1, &_eaddrnotavail_no, &_eaddrnotavail_msg, "");
#endif
#ifdef EAFNOSUPPORT
  set_ipc_err_vars (EAFNOSUPPORT, &_eafnosupport_no, &_eafnosupport_msg,
		    "address family not supported by");
#else
  set_ipc_err_vars (-1, &_eafnosupport_no, &_eafnosupport_msg, "");
#endif
#ifdef EALREADY
  set_ipc_err_vars (EALREADY, &_ealready_no, &_ealready_msg,
		    "operation already in progress");
#else
  set_ipc_err_vars (-1, &_ealready_no, &_ealready_msg, "");
#endif
#ifdef ECONNABORTED
  set_ipc_err_vars (ECONNABORTED, &_econnaborted_no, &_econnaborted_msg,
		    "software caused connection abort");
#else
  set_ipc_err_vars (-1, &_eonnaborted_no, &_eonnaborted_msg, "");
#endif
#ifdef ECONNREFUSED
  set_ipc_err_vars (ECONNREFUSED, &_econnrefused_no, &_econnrefused_msg,
		    "connection refused");
#else
  set_ipc_err_vars (-1, &_econnrefused_no, &_econnrefused_msg, "");
#endif
#ifdef ECONNRESET
  set_ipc_err_vars (ECONNRESET, &_econnreset_no, &_econnreset_msg,
		    "connection reset by peer");
#else
  set_ipc_err_vars (-1, &_econnreset_no, &_econnreset_msg, "");
#endif
#ifdef EDESTADDRREQ
  set_ipc_err_vars (EDESTADDRREQ, &_edestaddrreq_no, &_edestaddrreq_msg,
		    "destination address required");
#else
  set_ipc_err_vars (-1, &_edestaddrreq_no, &_edestaddrreq_msg, "");
#endif
#ifdef EHOSTDOWN
  set_ipc_err_vars (EHOSTDOWN, &_ehostdown_no, &_ehostdown_msg,
		    "host is down");
#else
  set_ipc_err_vars (-1, &_ehostdown_no, &_ehostdown_msg, "n");
#endif
#ifdef EHOSTUNREACH
  set_ipc_err_vars (EHOSTUNREACH, &_ehostunreach_no, &_ehostunreach_msg,
		    "no route to host");
#else
  set_ipc_err_vars (-1, &_ehostunreach_no, &_ehostunreach_msg, "");
#endif
#ifdef EINPROGRESS
  set_ipc_err_vars (EINPROGRESS, &_einprogress_no, &_einprogress_msg,
		    "operation now in progress");
#else
  set_ipc_err_vars (-1, &_einprogress_no, &_einprogress_msg, "");
#endif
#ifdef EISCONN
  set_ipc_err_vars (EISCONN, &_eisconn_no, &_eisconn_msg,
		    "socket is already connected");
#else
  set_ipc_err_vars (-1, &_eisconn_no, &_eisconn_msg, "");
#endif
#ifdef EMSGSIZE
  set_ipc_err_vars (EMSGSIZE, &_emsgsize_no, &_emsgsize_msg,
		    "message too long");
#else
  set_ipc_err_vars (-1, &_emsgsize_no, &_emsgsize_msg, "");
#endif
#ifdef ENETDOWN
  set_ipc_err_vars (ENETDOWN, &_enetdown_no, &_enetdown_msg,
		    "network is down");
#else
  set_ipc_err_vars (-1, &_enetdown_no, &_enetdown_msg, "");
#endif
#ifdef ENETRESET
  set_ipc_err_vars (ENETRESET, &_enetreset_no, &_enetreset_msg,
		    "network dropped connection because of reset");
#else
  set_ipc_err_vars (-1, &_enetreset_no, &_enetreset_msg, "");
#endif
#ifdef ENETUNREACH
  set_ipc_err_vars (ENETUNREACH, &_enetunreach_no, &_enetunreach_msg,
		    "network is unreachable");
#else
  set_ipc_err_vars (-1, &_enetunreach_no, &_enetunreach_msg, "");
#endif
#ifdef ENOBUFS
  set_ipc_err_vars (ENOBUFS, &_enobufs_no, &_enobufs_msg,
		    "no buffer space available");
#else
  set_ipc_err_vars (-1, &_enobufs_no, &_enobufs_msg, "");
#endif
#ifdef ENOPROTOOPT
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
#ifdef ENOTCONN
  set_ipc_err_vars (ENOTCONN, &_enotconn_no, &_enotconn_msg,
		    "socket is not connected");
#else
  set_ipc_err_vars (-1, &_enotconn_no, &_enotconn_msg, "");
#endif
#ifdef ENOTSOCK
  set_ipc_err_vars (ENOTSOCK, &_enotsock_no, &_enotsock_msg,
		    "socket operation on non-socket");
#else
  set_ipc_err_vars (-1, &_enotsock_no, &_enotsock_msg, "");
#endif
#ifdef EOPNOTSUPP
  set_ipc_err_vars (EOPNOTSUPP, &_eopnotsupp_no, &_eopnotsupp_msg,
		    "operation not supported on socket");
#else
  set_ipc_err_vars (-1, &_eopnotsupp_no, &_eopnotsupp_msg, "");
#endif
#ifdef EPFNOSUPPORT
  set_ipc_err_vars (EPFNOSUPPORT, &_epfnosupport_no, &_epfnosupport_msg,
		    "protocol family not supported");
#else
  set_ipc_err_vars (-1, &_epfnosupport_no, &_epfnosupport_msg, "");
#endif
#ifdef EPROTONOSUPPORT
  set_ipc_err_vars (EPROTONOSUPPORT, &_eprotonosupport_no,
		    &_eprotonosupport_msg, "protocol not supported");
#else
  set_ipc_err_vars (-1, &_eprotonosupport_no, &_eprotonosupport_msg, "");
#endif
#ifdef EPROTOTYPE
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
#ifdef ESHUTDOWN
  set_ipc_err_vars (ESHUTDOWN, &_eshutdown_no, &_eshutdown_msg,
		    "can't send after socket shutdown");
#else
  set_ipc_err_vars (-1, &_eshutdown_no, &_eshutdown_msg, "");
#endif
#ifdef ESOCKTNOSUPPORT
  set_ipc_err_vars (ESOCKTNOSUPPORT, &_esocktnosupport_no,
		    &_esocktnosupport_msg, "socket type not supported");
#else
  set_ipc_err_vars (-1, &_esocktnosupport_no, &_esocktnosupport_msg, "");
#endif
#ifdef ETIMEDOUT
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
#ifdef EWOULDBLOCK
#if defined(EAGAIN) && EAGAIN!=EWOULDBLOCK
  set_ipc_err_vars (EWOULDBLOCK, &_ewouldblock_no, &_ewouldblock_msg,
		    "operation would block");
#else
  set_ipc_err_vars (-1, &_ewouldblock_no, &_ewouldblock_msg, "");
#endif
  set_ipc_err_vars (-1, &_ewouldblock_no, &_ewouldblock_msg, "");
#endif
}

#ifndef WIN32
#if !defined(HAVE_DLOPEN) || defined(NO_EXTERN_SHLIB)

/* Function for implementing externals with static libraries.  See all
   externals name in ieee.d. */
void *
ipcerr_address (const char *name)
{
  if (strcmp (name, "_eaddrinuse_no") == 0)
    return _eaddrinuse_no;
  else if (strcmp (name, "_eaddrnotavail_no") == 0)
    return _eaddrnotavail_no;
  else if (strcmp (name, "_eafnosupport_no") == 0)
    return _eafnosupport_no;
  else if (strcmp (name, "_ealready_no") == 0)
    return _ealready_no;
  else if (strcmp (name, "_econnaborted_no") == 0)
    return _econnaborted_no;
  else if (strcmp (name, "_econnrefused_no") == 0)
    return _econnrefused_no;
  else if (strcmp (name, "_econnreset_no") == 0)
    return _econnreset_no;
  else if (strcmp (name, "_edestaddrreq_no") == 0)
    return _edestaddrreq_no;
  else if (strcmp (name, "_ehostdown_no") == 0)
    return _ehostdown_no;
  else if (strcmp (name, "_ehostunreach_no") == 0)
    return _ehostunreach_no;
  else if (strcmp (name, "_einprogress_no") == 0)
    return _einprogress_no;
  else if (strcmp (name, "_eisconn_no") == 0)
    return _eisconn_no;
  else if (strcmp (name, "_emsgsize_no") == 0)
    return _emsgsize_no;
  else if (strcmp (name, "_enetdown_no") == 0)
    return _enetdown_no;
  else if (strcmp (name, "_enetreset_no") == 0)
    return _enetreset_no;
  else if (strcmp (name, "_enetunreach_no") == 0)
    return _enetunreach_no;
  else if (strcmp (name, "_enobufs_no") == 0)
    return _enobufs_no;
  else if (strcmp (name, "_enoprotoopt_no") == 0)
    return _enoprotoopt_no;
  else if (strcmp (name, "_enosr_no") == 0)
    return _enosr_no;
  else if (strcmp (name, "_enotconn_no") == 0)
    return _enotconn_no;
  else if (strcmp (name, "_enotsock_no") == 0)
    return _enotsock_no;
  else if (strcmp (name, "_eopnotsupp_no") == 0)
    return _eopnotsupp_no;
  else if (strcmp (name, "_epfnosupport_no") == 0)
    return _epfnosupport_no;
  else if (strcmp (name, "_eprotonosupport_no") == 0)
    return _eprotonosupport_no;
  else if (strcmp (name, "_eprototype_no") == 0)
    return _eprototype_no;
  else if (strcmp (name, "_eremoterelease_no") == 0)
    return _eremoterelease_no;
  else if (strcmp (name, "_eshutdown_no") == 0)
    return _eshutdown_no;
  else if (strcmp (name, "_esocktnosupport_no") == 0)
    return _esocktnosupport_no;
  else if (strcmp (name, "_etimedout_no") == 0)
    return _etimedout_no;
  else if (strcmp (name, "_etoomanyrefs_no") == 0)
    return _etoomanyrefs_no;
  else if (strcmp (name, "_ewouldblock_no") == 0)
    return _ewouldblock_no;
  else if (strcmp (name, "_eaddrinuse_msg") == 0)
    return _eaddrinuse_msg;
  else if (strcmp (name, "_eaddrnotavail_msg") == 0)
    return _eaddrnotavail_msg;
  else if (strcmp (name, "_eafnosupport_msg") == 0)
    return _eafnosupport_msg;
  else if (strcmp (name, "_ealready_msg") == 0)
    return _ealready_msg;
  else if (strcmp (name, "_econnaborted_msg") == 0)
    return _econnaborted_msg;
  else if (strcmp (name, "_econnrefused_msg") == 0)
    return _econnrefused_msg;
  else if (strcmp (name, "_econnreset_msg") == 0)
    return _econnreset_msg;
  else if (strcmp (name, "_edestaddrreq_msg") == 0)
    return _edestaddrreq_msg;
  else if (strcmp (name, "_ehostdown_msg") == 0)
    return _ehostdown_msg;
  else if (strcmp (name, "_ehostunreach_msg") == 0)
    return _ehostunreach_msg;
  else if (strcmp (name, "_einprogress_msg") == 0)
    return _einprogress_msg;
  else if (strcmp (name, "_eisconn_msg") == 0)
    return _eisconn_msg;
  else if (strcmp (name, "_emsgsize_msg") == 0)
    return _emsgsize_msg;
  else if (strcmp (name, "_enetdown_msg") == 0)
    return _enetdown_msg;
  else if (strcmp (name, "_enetreset_msg") == 0)
    return _enetreset_msg;
  else if (strcmp (name, "_enetunreach_msg") == 0)
    return _enetunreach_msg;
  else if (strcmp (name, "_enobufs_msg") == 0)
    return _enobufs_msg;
  else if (strcmp (name, "_enoprotoopt_msg") == 0)
    return _enoprotoopt_msg;
  else if (strcmp (name, "_enosr_msg") == 0)
    return _enosr_msg;
  else if (strcmp (name, "_enotconn_msg") == 0)
    return _enotconn_msg;
  else if (strcmp (name, "_enotsock_msg") == 0)
    return _enotsock_msg;
  else if (strcmp (name, "_eopnotsupp_msg") == 0)
    return _eopnotsupp_msg;
  else if (strcmp (name, "_epfnosupport_msg") == 0)
    return _epfnosupport_msg;
  else if (strcmp (name, "_eprotonosupport_msg") == 0)
    return _eprotonosupport_msg;
  else if (strcmp (name, "_eprototype_msg") == 0)
    return _eprototype_msg;
  else if (strcmp (name, "_eremoterelease_msg") == 0)
    return _eremoterelease_msg;
  else if (strcmp (name, "_eshutdown_msg") == 0)
    return _eshutdown_msg;
  else if (strcmp (name, "_esocktnosupport_msg") == 0)
    return _esocktnosupport_msg;
  else if (strcmp (name, "_etimedout_msg") == 0)
    return _etimedout_msg;
  else if (strcmp (name, "_etoomanyrefs_msg") == 0)
    return _etoomanyrefs_msg;
  else if (strcmp (name, "_ewouldblock_msg") == 0)
    return _ewouldblock_msg;
  else if  (strcmp (name, "_ipc_err_init") == 0)
    return _ipc_err_init;
  else
    return NULL;
}
#endif
#endif
