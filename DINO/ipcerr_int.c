#ifdef HAVE_CONFIG_H
#include "d_config.h"
#endif /* #ifdef HAVE_CONFIG_H */
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
  _ewouldblock_no, _eos_specific_nos;

WIN_EXPORT val_t _eaddrinuse_msg, _eaddrnotavail_msg, _eafnosupport_msg,
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
  ER_set_vect ((ER_node_t) val_msg, vect);
}

static void
set_ipc_os_error (int no, const char *msg)
{
  ER_node_t vect, msg_vect;

  vect = ER_vect ((ER_node_t) &_eos_specific_nos);
  ((int_t *) ER_pack_els (vect)) [ER_els_number (vect)] = no;
  ER_set_els_number (vect, ER_els_number (vect) + 1);
  msg_vect = create_string (msg);
  vect = ER_vect ((ER_node_t) &_eos_specific_msgs);
  ((ER_node_t *) ER_pack_els (vect)) [ER_els_number (vect)] = msg_vect;
  ER_set_els_number (vect, ER_els_number (vect) + 1);
}

WIN_EXPORT val_t
_ipc_err_init (int npars, val_t *vals)
{
  ER_node_t vect;
  val_t val;
  ER_node_t res = (ER_node_t) &val;

  assert (npars == 0);

#if defined(WIN32) && defined(WSAEADDRINUSE)
  set_ipc_err_vars (WSAEADDRINUSE, &_eaddrinuse_no, &_eaddrinuse_msg,
		    "address already in use");
#elif defined(EADDRINUSE)
  set_ipc_err_vars (EADDRINUSE, &_eaddrinuse_no, &_eaddrinuse_msg,
		    "address already in use");
#else
  set_ipc_err_vars (-1, &_eaddrinuse_no, &_eaddrinuse_msg, "");
#endif
  
#if defined(WIN32) && defined(WSAEADDRNOTAVAIL)
  set_ipc_err_vars (WSAEADDRNOTAVAIL, &_eaddrnotavail_no, &_eaddrnotavail_msg,
		    "can't assign requested address");
#elif defined(EADDRNOTAVAIL)
  set_ipc_err_vars (EADDRNOTAVAIL, &_eaddrnotavail_no, &_eaddrnotavail_msg,
		    "can't assign requested address");
#else
  set_ipc_err_vars (-1, &_eaddrnotavail_no, &_eaddrnotavail_msg, "");
#endif

#if defined(WIN32) && defined(WSAEAFNOSUPPORT)
  set_ipc_err_vars (WSAEAFNOSUPPORT, &_eafnosupport_no, &_eafnosupport_msg,
		    "address family not supported by protocol family");
#elif defined(EAFNOSUPPORT)
  set_ipc_err_vars (EAFNOSUPPORT, &_eafnosupport_no, &_eafnosupport_msg,
		    "address family not supported by protocol family");
#else
  set_ipc_err_vars (-1, &_eafnosupport_no, &_eafnosupport_msg, "");
#endif

#if defined(WIN32) && defined(WSAEALEADY)
  set_ipc_err_vars (WSAEALREADY, &_ealready_no, &_ealready_msg,
		    "operation already in progress");
#elif defined(EALREADY)
  set_ipc_err_vars (EALREADY, &_ealready_no, &_ealready_msg,
		    "operation already in progress");
#else
  set_ipc_err_vars (-1, &_ealready_no, &_ealready_msg, "");
#endif

#if defined(WIN32) && defined(WSAECONNABORTED)
  set_ipc_err_vars (WSAECONNABORTED, &_econnaborted_no, &_econnaborted_msg,
		    "software caused connection abort");
#elif defined(ECONNABORTED)
  set_ipc_err_vars (ECONNABORTED, &_econnaborted_no, &_econnaborted_msg,
		    "software caused connection abort");
#else
  set_ipc_err_vars (-1, &_econnaborted_no, &_econnaborted_msg, "");
#endif

#if defined(WIN32) && defined (WSAECONNREFUSED)
  set_ipc_err_vars (WSAECONNREFUSED, &_econnrefused_no, &_econnrefused_msg,
		    "connection refused");
#elif defined(ECONNREFUSED)
  set_ipc_err_vars (ECONNREFUSED, &_econnrefused_no, &_econnrefused_msg,
		    "connection refused");
#else
  set_ipc_err_vars (-1, &_econnrefused_no, &_econnrefused_msg, "");
#endif

#if defined(WIN32) && defined (WSAECONNRESET)
  set_ipc_err_vars (WSAECONNRESET, &_econnreset_no, &_econnreset_msg,
		    "connection reset by peer");
#elif defined(ECONNRESET)
  set_ipc_err_vars (ECONNRESET, &_econnreset_no, &_econnreset_msg,
		    "connection reset by peer");
#else
  set_ipc_err_vars (-1, &_econnreset_no, &_econnreset_msg, "");
#endif

#if defined(WIN32) && defined (WSAEDESTADDRREQ)
  set_ipc_err_vars (WSAEDESTADDRREQ, &_edestaddrreq_no, &_edestaddrreq_msg,
		    "destination address required");
#elif defined(EDESTADDRREQ)
  set_ipc_err_vars (EDESTADDRREQ, &_edestaddrreq_no, &_edestaddrreq_msg,
		    "destination address required");
#else
  set_ipc_err_vars (-1, &_edestaddrreq_no, &_edestaddrreq_msg, "");
#endif

#if defined(WIN32) && defined (WSAEHOSTDOWN)
  set_ipc_err_vars (WSAEHOSTDOWN, &_ehostdown_no, &_ehostdown_msg,
		    "host is down");
#elif defined(EHOSTDOWN)
  set_ipc_err_vars (EHOSTDOWN, &_ehostdown_no, &_ehostdown_msg,
		    "host is down");
#else
  set_ipc_err_vars (-1, &_ehostdown_no, &_ehostdown_msg, "n");
#endif

#if defined(WIN32) && defined (WSAEHOSTUNREACH)
  set_ipc_err_vars (WSAEHOSTUNREACH, &_ehostunreach_no, &_ehostunreach_msg,
		    "no route to host");
#elif defined(EHOSTUNREACH)
  set_ipc_err_vars (EHOSTUNREACH, &_ehostunreach_no, &_ehostunreach_msg,
		    "no route to host");
#else
  set_ipc_err_vars (-1, &_ehostunreach_no, &_ehostunreach_msg, "");
#endif

#if defined(WIN32) && defined (WSAEINPROGRESS)
  set_ipc_err_vars (WSAEINPROGRESS, &_einprogress_no, &_einprogress_msg,
		    "operation now in progress");
#elif defined(EINPROGRESS)
  set_ipc_err_vars (EINPROGRESS, &_einprogress_no, &_einprogress_msg,
		    "operation now in progress");
#else
  set_ipc_err_vars (-1, &_einprogress_no, &_einprogress_msg, "");
#endif

#if defined(WIN32) && defined (WSAEISCONN)
  set_ipc_err_vars (WSAEISCONN, &_eisconn_no, &_eisconn_msg,
		    "socket is already connected");
#elif defined(EISCONN)
  set_ipc_err_vars (EISCONN, &_eisconn_no, &_eisconn_msg,
		    "socket is already connected");
#else
  set_ipc_err_vars (-1, &_eisconn_no, &_eisconn_msg, "");
#endif

#if defined(WIN32) && defined (WSAEMSGSIZE)
  set_ipc_err_vars (WSAEMSGSIZE, &_emsgsize_no, &_emsgsize_msg,
		    "message too long");
#elif defined(EMSGSIZE)
  set_ipc_err_vars (EMSGSIZE, &_emsgsize_no, &_emsgsize_msg,
		    "message too long");
#else
  set_ipc_err_vars (-1, &_emsgsize_no, &_emsgsize_msg, "");
#endif

#if defined(WIN32) && defined (WSAENETDOWN)
  set_ipc_err_vars (WSAENETDOWN, &_enetdown_no, &_enetdown_msg,
		    "network is down");
#elif defined(ENETDOWN)
  set_ipc_err_vars (ENETDOWN, &_enetdown_no, &_enetdown_msg,
		    "network is down");
#else
  set_ipc_err_vars (-1, &_enetdown_no, &_enetdown_msg, "");
#endif

#if defined(WIN32) && defined (WSAENETRESET)
  set_ipc_err_vars (WSAENETRESET, &_enetreset_no, &_enetreset_msg,
		    "network dropped connection because of reset");
#elif defined(ENETRESET)
  set_ipc_err_vars (ENETRESET, &_enetreset_no, &_enetreset_msg,
		    "network dropped connection because of reset");
#else
  set_ipc_err_vars (-1, &_enetreset_no, &_enetreset_msg, "");
#endif

#if defined(WIN32) && defined (WSAENETUNREACH)
  set_ipc_err_vars (WSAENETUNREACH, &_enetunreach_no, &_enetunreach_msg,
		    "network is unreachable");
#elif defined(ENETUNREACH)
  set_ipc_err_vars (ENETUNREACH, &_enetunreach_no, &_enetunreach_msg,
		    "network is unreachable");
#else
  set_ipc_err_vars (-1, &_enetunreach_no, &_enetunreach_msg, "");
#endif

#if defined(WIN32) && defined (WSAENOBUFS)
  set_ipc_err_vars (WSAENOBUFS, &_enobufs_no, &_enobufs_msg,
		    "no buffer space available");
#elif defined(ENOBUFS)
  set_ipc_err_vars (ENOBUFS, &_enobufs_no, &_enobufs_msg,
		    "no buffer space available");
#else
  set_ipc_err_vars (-1, &_enobufs_no, &_enobufs_msg, "");
#endif

#if defined(WIN32) && defined (WSAENOPROTOOPT)
  set_ipc_err_vars (WSAENOPROTOOPT, &_enoprotoopt_no, &_enoprotoopt_msg,
		    "protocol not available");
#elif defined(ENOPROTOOPT)
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

#if defined(WIN32) && defined (WSAENOTCONN)
  set_ipc_err_vars (WSAENOTCONN, &_enotconn_no, &_enotconn_msg,
		    "socket is not connected");
#elif defined(ENOTCONN)
  set_ipc_err_vars (ENOTCONN, &_enotconn_no, &_enotconn_msg,
		    "socket is not connected");
#else
  set_ipc_err_vars (-1, &_enotconn_no, &_enotconn_msg, "");
#endif

#if defined(WIN32) && defined (WSAENOTSOCK)
  set_ipc_err_vars (WSAENOTSOCK, &_enotsock_no, &_enotsock_msg,
		    "socket operation on non-socket");
#elif defined(ENOTSOCK)
  set_ipc_err_vars (ENOTSOCK, &_enotsock_no, &_enotsock_msg,
		    "socket operation on non-socket");
#else
  set_ipc_err_vars (-1, &_enotsock_no, &_enotsock_msg, "");
#endif

#if defined(WIN32) && defined (WSAEOPNOTSUPP)
  set_ipc_err_vars (WSAEOPNOTSUPP, &_eopnotsupp_no, &_eopnotsupp_msg,
		    "operation not supported on socket");
#elif defined(EOPNOTSUPP)
  set_ipc_err_vars (EOPNOTSUPP, &_eopnotsupp_no, &_eopnotsupp_msg,
		    "operation not supported on socket");
#else
  set_ipc_err_vars (-1, &_eopnotsupp_no, &_eopnotsupp_msg, "");
#endif

#if defined(WIN32) && defined (WSAEPFNOSUPPORT)
  set_ipc_err_vars (WSAEPFNOSUPPORT, &_epfnosupport_no, &_epfnosupport_msg,
		    "protocol family not supported");
#elif defined(EPFNOSUPPORT)
  set_ipc_err_vars (EPFNOSUPPORT, &_epfnosupport_no, &_epfnosupport_msg,
		    "protocol family not supported");
#else
  set_ipc_err_vars (-1, &_epfnosupport_no, &_epfnosupport_msg, "");
#endif

#if defined(WIN32) && defined (WSAEPROTONOSUPPORT)
  set_ipc_err_vars (WSAEPROTONOSUPPORT, &_eprotonosupport_no,
		    &_eprotonosupport_msg, "protocol not supported");
#elif defined(EPROTONOSUPPORT)
  set_ipc_err_vars (EPROTONOSUPPORT, &_eprotonosupport_no,
		    &_eprotonosupport_msg, "protocol not supported");
#else
  set_ipc_err_vars (-1, &_eprotonosupport_no, &_eprotonosupport_msg, "");
#endif

#if defined(WIN32) && defined (WSAEPROTOTYPE)
  set_ipc_err_vars (WSAEPROTOTYPE, &_eprototype_no, &_eprototype_msg,
		    "protocol wrong type for socket");
#elif defined(EPROTOTYPE)
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

#if defined(WIN32) && defined (WSAESHUTDOWN)
  set_ipc_err_vars (WSAESHUTDOWN, &_eshutdown_no, &_eshutdown_msg,
		    "can't send after socket shutdown");
#elif defined(ESHUTDOWN)
  set_ipc_err_vars (ESHUTDOWN, &_eshutdown_no, &_eshutdown_msg,
		    "can't send after socket shutdown");
#else
  set_ipc_err_vars (-1, &_eshutdown_no, &_eshutdown_msg, "");
#endif

#if defined(WIN32) && defined (WSAESOCKTNOSUPPORT)
  set_ipc_err_vars (WSAESOCKTNOSUPPORT, &_esocktnosupport_no,
		    &_esocktnosupport_msg, "socket type not supported");
#elif defined(ESOCKTNOSUPPORT)
  set_ipc_err_vars (ESOCKTNOSUPPORT, &_esocktnosupport_no,
		    &_esocktnosupport_msg, "socket type not supported");
#else
  set_ipc_err_vars (-1, &_esocktnosupport_no, &_esocktnosupport_msg, "");
#endif

#if defined(WIN32) && defined (WSAETIMEDOUT)
  set_ipc_err_vars (WSAETIMEDOUT, &_etimedout_no, &_etimedout_msg,
		    "connection timed out");
#elif defined(ETIMEDOUT)
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

#if defined(WIN32) && defined (WSAEWOULDBLOCK)
  set_ipc_err_vars (WSAEWOULDBLOCK, &_ewouldblock_no, &_ewouldblock_msg,
		    "operation would block");
#elif defined(EWOULDBLOCK)
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

#ifdef WIN32

#define MAX_OS_ERROR_VECT_LENGTH 100

  vect = create_pack_vector (MAX_OS_ERROR_VECT_LENGTH, ER_NM_int);
  ER_set_els_number (vect, 0);
  ER_SET_MODE ((ER_node_t) &_eos_specific_nos, ER_NM_vect);
  ER_set_vect ((ER_node_t) &_eos_specific_nos, vect);

  vect = create_pack_vector (MAX_OS_ERROR_VECT_LENGTH, ER_NM_vect);
  ER_set_els_number (vect, 0);
  ER_SET_MODE ((ER_node_t) &_eos_specific_msgs, ER_NM_vect);
  ER_set_vect ((ER_node_t) &_eos_specific_msgs, vect);

  
#if defined(WSAEACCES)
  set_ipc_os_error (WSAEACCES, "Permission denied");
#endif

#if defined(WSAEFAULT)
  set_ipc_os_error (WSAEFAULT, "Bad address");
#endif

#if defined(WSAEINTR)
  set_ipc_os_error (WSAEINTR, "Interrupted function call");
#endif

#if defined(WSAEINVAL)
  set_ipc_os_error (WSAEINVAL, "Invalid argument");
#endif

#if defined(WSAEMFILE)
  set_ipc_os_error (WSAEMFILE, "Too many open files");
#endif

#if defined(WSAEPROCLIM)
  set_ipc_os_error (WSAEPROCLIM, "Too many processes");
#endif

#if defined(WSATYPE_NOT_FOUND)
  set_ipc_os_error (WSATYPE_NOT_FOUND, "Class type not found");
#endif

#if defined(WSAHOST_NOT_FOUND)
  set_ipc_os_error (WSAHOST_NOT_FOUND, "Host not found");
#endif

#if defined(WSA_INVALID_HANDLE)
  set_ipc_os_error (WSA_INVALID_HANDLE, "Specified event object handle is invalid");
#endif

#if defined(WSA_INVALID_PARAMETER)
  set_ipc_os_error (WSA_INVALID_PARAMETER, "One or more parameters are invalid");
#endif

#if defined(WSAINVALIDPROCTABLE)
  set_ipc_os_error (WSAINVALIDPROCTABLE, "Invalid procedure table from service provider");
#endif

#if defined(WSAINVALIDPROVIDER)
  set_ipc_os_error (WSAINVALIDPROVIDER, "Invalid service provider version number");
#endif

#if defined(WSA_IO_INCOMPLETE)
  set_ipc_os_error (WSA_IO_INCOMPLETE, "Overlapped I/O event object not in signaled stat");
#endif

#if defined(WSA_NOT_ENOUGH_MEMORY)
  set_ipc_os_error (WSA_NOT_ENOUGH_MEMORY, "Insufficient memory available");
#endif

#if defined(WSANOTINITIALISED)
  set_ipc_os_error (WSANOTINITIALISED, "Successful WSAStartup not yet performed");
#endif

#if defined(WSANO_DATA)
  set_ipc_os_error (WSANO_DATA, "Valid name, no data record of requested type");
#endif

#if defined(WSANO_RECOVERY)
  set_ipc_os_error (WSANO_RECOVERY, "This is a non-recoverable error");
#endif

#if defined(WSAPROVIDERFAILEDINIT)
  set_ipc_os_error (WSAPROVIDERFAILEDINIT, "Unable to initialize a service provider");
#endif

#if defined(WSASYSCALLFAILURE)
  set_ipc_os_error (WSASYSCALLFAILURE, "System call failure");
#endif

#if defined(WSASYSNOTREADY)
  set_ipc_os_error (WSASYSNOTREADY, "Network subsystem is unavailable");
#endif

#if defined(WSATRY_AGAIN)
  set_ipc_os_error (WSATRY_AGAIN, "Non-authoritative host not found");
#endif

#if defined(WSAVERNOTSUPPORTED)
  set_ipc_os_error (WSAVERNOTSUPPORTED, "WINSOCK.DLL version out of range");
#endif

#if defined(WSAEDISCON)
  set_ipc_os_error (WSAEDISCON, "Graceful shutdown in progress");
#endif

#if defined(WSA_OPERATION_ABORTED)
  set_ipc_os_error (WSA_OPERATION_ABORTED, "Overlapped operation aborted");
#endif

#endif
  ER_SET_MODE (res, ER_NM_nil);
  return val;
}

#ifndef WIN32
#if !defined(HAVE_DLOPEN) || defined(NO_EXTERN_SHLIB)

/* Function for implementing externals with static libraries.  See all
   externals name in ieee.d. */
void *
ipcerr_address (const char *name)
{
  if (strcmp (name, "_eaddrinuse_no") == 0)
    return &_eaddrinuse_no;
  else if (strcmp (name, "_eaddrnotavail_no") == 0)
    return &_eaddrnotavail_no;
  else if (strcmp (name, "_eafnosupport_no") == 0)
    return &_eafnosupport_no;
  else if (strcmp (name, "_ealready_no") == 0)
    return &_ealready_no;
  else if (strcmp (name, "_econnaborted_no") == 0)
    return &_econnaborted_no;
  else if (strcmp (name, "_econnrefused_no") == 0)
    return &_econnrefused_no;
  else if (strcmp (name, "_econnreset_no") == 0)
    return &_econnreset_no;
  else if (strcmp (name, "_edestaddrreq_no") == 0)
    return &_edestaddrreq_no;
  else if (strcmp (name, "_ehostdown_no") == 0)
    return &_ehostdown_no;
  else if (strcmp (name, "_ehostunreach_no") == 0)
    return &_ehostunreach_no;
  else if (strcmp (name, "_einprogress_no") == 0)
    return &_einprogress_no;
  else if (strcmp (name, "_eisconn_no") == 0)
    return &_eisconn_no;
  else if (strcmp (name, "_emsgsize_no") == 0)
    return &_emsgsize_no;
  else if (strcmp (name, "_enetdown_no") == 0)
    return &_enetdown_no;
  else if (strcmp (name, "_enetreset_no") == 0)
    return &_enetreset_no;
  else if (strcmp (name, "_enetunreach_no") == 0)
    return &_enetunreach_no;
  else if (strcmp (name, "_enobufs_no") == 0)
    return &_enobufs_no;
  else if (strcmp (name, "_enoprotoopt_no") == 0)
    return &_enoprotoopt_no;
  else if (strcmp (name, "_enosr_no") == 0)
    return &_enosr_no;
  else if (strcmp (name, "_enotconn_no") == 0)
    return &_enotconn_no;
  else if (strcmp (name, "_enotsock_no") == 0)
    return &_enotsock_no;
  else if (strcmp (name, "_eopnotsupp_no") == 0)
    return &_eopnotsupp_no;
  else if (strcmp (name, "_epfnosupport_no") == 0)
    return &_epfnosupport_no;
  else if (strcmp (name, "_eprotonosupport_no") == 0)
    return &_eprotonosupport_no;
  else if (strcmp (name, "_eprototype_no") == 0)
    return &_eprototype_no;
  else if (strcmp (name, "_eremoterelease_no") == 0)
    return &_eremoterelease_no;
  else if (strcmp (name, "_eshutdown_no") == 0)
    return &_eshutdown_no;
  else if (strcmp (name, "_esocktnosupport_no") == 0)
    return &_esocktnosupport_no;
  else if (strcmp (name, "_etimedout_no") == 0)
    return &_etimedout_no;
  else if (strcmp (name, "_etoomanyrefs_no") == 0)
    return &_etoomanyrefs_no;
  else if (strcmp (name, "_ewouldblock_no") == 0)
    return &_ewouldblock_no;
  else if (strcmp (name, "_eos_specific_nos") == 0)
    return &_eos_specific_nos;
  else if (strcmp (name, "_eaddrinuse_msg") == 0)
    return &_eaddrinuse_msg;
  else if (strcmp (name, "_eaddrnotavail_msg") == 0)
    return &_eaddrnotavail_msg;
  else if (strcmp (name, "_eafnosupport_msg") == 0)
    return &_eafnosupport_msg;
  else if (strcmp (name, "_ealready_msg") == 0)
    return &_ealready_msg;
  else if (strcmp (name, "_econnaborted_msg") == 0)
    return &_econnaborted_msg;
  else if (strcmp (name, "_econnrefused_msg") == 0)
    return &_econnrefused_msg;
  else if (strcmp (name, "_econnreset_msg") == 0)
    return &_econnreset_msg;
  else if (strcmp (name, "_edestaddrreq_msg") == 0)
    return &_edestaddrreq_msg;
  else if (strcmp (name, "_ehostdown_msg") == 0)
    return &_ehostdown_msg;
  else if (strcmp (name, "_ehostunreach_msg") == 0)
    return &_ehostunreach_msg;
  else if (strcmp (name, "_einprogress_msg") == 0)
    return &_einprogress_msg;
  else if (strcmp (name, "_eisconn_msg") == 0)
    return &_eisconn_msg;
  else if (strcmp (name, "_emsgsize_msg") == 0)
    return &_emsgsize_msg;
  else if (strcmp (name, "_enetdown_msg") == 0)
    return &_enetdown_msg;
  else if (strcmp (name, "_enetreset_msg") == 0)
    return &_enetreset_msg;
  else if (strcmp (name, "_enetunreach_msg") == 0)
    return &_enetunreach_msg;
  else if (strcmp (name, "_enobufs_msg") == 0)
    return &_enobufs_msg;
  else if (strcmp (name, "_enoprotoopt_msg") == 0)
    return &_enoprotoopt_msg;
  else if (strcmp (name, "_enosr_msg") == 0)
    return &_enosr_msg;
  else if (strcmp (name, "_enotconn_msg") == 0)
    return &_enotconn_msg;
  else if (strcmp (name, "_enotsock_msg") == 0)
    return &_enotsock_msg;
  else if (strcmp (name, "_eopnotsupp_msg") == 0)
    return &_eopnotsupp_msg;
  else if (strcmp (name, "_epfnosupport_msg") == 0)
    return &_epfnosupport_msg;
  else if (strcmp (name, "_eprotonosupport_msg") == 0)
    return &_eprotonosupport_msg;
  else if (strcmp (name, "_eprototype_msg") == 0)
    return &_eprototype_msg;
  else if (strcmp (name, "_eremoterelease_msg") == 0)
    return &_eremoterelease_msg;
  else if (strcmp (name, "_eshutdown_msg") == 0)
    return &_eshutdown_msg;
  else if (strcmp (name, "_esocktnosupport_msg") == 0)
    return &_esocktnosupport_msg;
  else if (strcmp (name, "_etimedout_msg") == 0)
    return &_etimedout_msg;
  else if (strcmp (name, "_etoomanyrefs_msg") == 0)
    return &_etoomanyrefs_msg;
  else if (strcmp (name, "_ewouldblock_msg") == 0)
    return &_ewouldblock_msg;
  else if (strcmp (name, "_eos_specific_msgs") == 0)
    return &_eos_specific_msgs;
  else if  (strcmp (name, "_ipc_err_init") == 0)
    return _ipc_err_init;
  else
    return NULL;
}
#endif
#endif
