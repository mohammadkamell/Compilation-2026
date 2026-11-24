#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "runtime.h"

struct array *c_string_to_dolphin_string(char *cs){
  int64_t len = strlen(cs);
  struct array *ds = raw_allocate_array(sizeof(char), len);
  ds->len = len;
  memcpy(ds->contents, cs, ds->len);
  return ds;
}

char *dolphin_string_to_c_string(struct array *ds){
  char *cs = raw_allocate_on_heap(sizeof(char) * ds->len + 1);
  memcpy(cs, ds->contents, ds->len);
  cs[ds->len] = 0;
  return cs;
}

struct stream; // wrapper for FILE

struct stream *get_stdout(){
  return (struct stream *) stdout;
}

struct stream *get_stderr(){
  return (struct stream *) stderr;
}

struct stream *get_stdin(){
  return (struct stream *) stdin;
}

int64_t get_eof(){
  return EOF;
}

FILE *cast_file(struct stream *f){
  if (f == NULL){ report_error_nil_access(); }
  return (FILE *) f;
}

char flush_file(struct stream *f){
  FILE *F = cast_file(f);
  if(fflush(F) == EOF) return 0; else return 1;
}

struct stream *open_file(struct array *path, struct array *mode){
  char *cpath = dolphin_string_to_c_string(path);
  char *cmode = dolphin_string_to_c_string(mode);
  return (struct stream *) fopen(cpath, cmode);
}

char end_of_file(struct stream *f){
  FILE *F = cast_file(f);
  if(feof(F)) return 1; else return 0;
}

char error_in_file(struct stream *f){
  FILE *F = cast_file(f);
  if(ferror(F)) return 1; else return 0;
}

char close_file(struct stream *f){
  FILE *F = cast_file(f);
  if(fclose(F) == EOF) return 0; else return 1;
}

void output_raw(const char *s, int64_t len, FILE *f) {
  for (int64_t i = 0; i < len; i++, s++){
      fputc(*s, f);
  }
}

void output_bytes_array(struct array *a, struct stream *f) {
  if (a == NULL){ report_error_nil_access(); }
  FILE *F = cast_file(f);
  output_raw(a->contents, a->len, F);
}

void output_string(struct array *s, struct stream *f) {
  output_bytes_array(s, f);
}

struct array *input_bytes_array(int64_t len, struct stream *f){
  FILE *F = cast_file(f);
  int64_t read = 0;
  char tmp = 0;
  struct array *res = allocate_array(sizeof(char),len, &tmp);
  char *head = res->contents;
  for(char c;read < len;read++){
    c = fgetc(F);
    if (c == EOF)
      break;
    *(head++) = c;
  }
  res->len = read;
  return res;
}

char output_byte(char c, struct stream *f) {
    FILE *F = cast_file(f);
    if(fputc(c, F) == EOF) return 0; else return 1;
}

int64_t input_byte(struct stream *f){
  FILE *F = cast_file(f);
  return fgetc(F);
}

int64_t pos_in_file(struct stream *f){
  FILE *F = cast_file(f);
  return ftell(F);
}

char seek_in_file(int64_t relpos, char relative, struct stream *f){
  FILE *F = cast_file(f);
  int origin = relative ? SEEK_CUR : SEEK_SET;
  return fseek(F, relpos, origin);
}

int64_t string_to_int(struct array *s){
  if (s == NULL){ report_error_nil_access(); }
  char *str = dolphin_string_to_c_string(s);
  int64_t res = strtoll(str, NULL, 10);
  free(str);
  return res;
}

struct array *int_to_string(int64_t i){
  struct array *res = raw_allocate_array(sizeof(char), 24);
  res->contents[0] = 0;
  int64_t len = sprintf(res->contents, "%" PRId64, i);
  res->len = len;
  return res;
}

struct array *substring(struct array *s, int64_t start, int64_t len){
  if (s == NULL){ report_error_nil_access(); }
  if(start < 0 || start >= s->len){
    fprintf(stderr, "stdlib error: start of substring must be within the string.\n");
    exit(1);
  }
  int64_t available = s->len - start;
  int64_t l = len < 0LL ? 0 : len;
  l = l > available ? available : l;
  struct array * res = raw_allocate_array(sizeof(char), l);
  res-> len = l;
  memcpy(res->contents, s->contents + start, l);
  return res;
}

struct array *string_concat(struct array *s1, struct array *s2){
  if (s1 == NULL || s2 == NULL){ report_error_nil_access(); }
  int64_t len = s1->len + s2->len;
  struct array *res = raw_allocate_array(sizeof(char), len);
  res->len = len;
  memcpy(res->contents, s1->contents, s1->len);
  memcpy((res->contents) + s1->len, s2->contents, s2->len);
  return res;
}

struct array *ascii_chr(int64_t c){
  if(c > 255LL || c < 0LL){
    fprintf(stderr, "stdlib error: character code must be between 0 and 255.\n");
    exit(1);
  }
  struct array * res = raw_allocate_array(sizeof(char), 1);
  res->contents[0] = c;
  return res;
}

int64_t ascii_ord(struct array *s){
  if (s == NULL){ report_error_nil_access(); }
  if (s->len != 1){
    fprintf(stderr, "stdlib error: the function ascii_ord must only be called on strings consisting of a single character.\n");
    exit(1);
  }
  return s->contents[0];
}

int64_t byte_to_int_signed(char c){
  return c;
}

int64_t byte_to_int_unsigned(unsigned char c){
  return c;
}

char int_to_byte_signed(int64_t i){
  if (i > 127 || i < -128){
    fprintf(stderr, "stdlib error: int_to_byte_signed can only be applied to integers in the range -128 to 127 (both included).\n");
    exit(1);
  }
  return i;
}

unsigned char int_to_byte_unsigned(int64_t i){
  if (i > 255 || i < 0){
    fprintf(stderr, "stdlib error: int_to_byte_unsigned can only be applied to integers in the range 0 to 255 (both included).\n");
    exit(1);
  }
  return i;
}

struct array *string_to_bytes_array(struct array *s){
  if (s == NULL){ report_error_nil_access(); }
  char tmp = 0;
  struct array *res = allocate_array(sizeof(char), s->len, &tmp);
  memcpy(res->contents, s->contents, s->len);
  return res;
}

struct array *bytes_array_to_string(struct array *a){
  if (a == NULL){ report_error_nil_access(); }
  struct array *res = raw_allocate_array(sizeof(char), a->len);
  memcpy(res->contents, a->contents, res->len);
  return res;
}

// networking.

enum socket_status {FRESH_SOCKET, BOUND_SOCKET, LISTENING_SOCKET, CONNECTED_SOCKET, ACTIVE_UDP_SOCKET};

struct connection_type {};
struct internal_connection_type {int type;} tcp_type = {1}, udp_type = {2};

struct ip_version {};
struct internal_ip_version {int version;} ipv4 = {1}, ipv6 = {2};

struct socket {};
struct internal_socket {int socfd; struct internal_connection_type *ctype; struct internal_ip_version *ip_ver; enum socket_status status; struct stream *input_stream; struct stream *output_stream;};

struct ip_address {};
int IP_ADDRESS_ANY_INITIALIZED = 0;
struct internal_ip_address {struct internal_ip_version *ip_ver; void *addr;} ipv4_address_any = {}, ipv6_address_any = {};

struct socket_address {};
struct internal_socket_address {struct internal_ip_version *ip_ver; struct sockaddr *raw_socket_address;};

struct accepted_connection {struct socket *connection; struct socket_address *other_side;};

struct udp_recvfrom_result {struct array *data; struct socket_address *sender;};

struct connection_type *get_udp_connection_type(){ return (struct connection_type *) &udp_type; }
struct connection_type *get_tcp_connection_type(){ return (struct connection_type *) &tcp_type; }

struct ip_version *get_ipv4(){ return (struct ip_version *) &ipv4; }
struct ip_version *get_ipv6(){ return (struct ip_version *) &ipv6; }

void initialize_ip_address_any(){
  ipv4_address_any.ip_ver = &ipv4;
  ipv4_address_any.addr = raw_allocate_on_heap(sizeof (struct in_addr));
  ((struct in_addr *) ipv4_address_any.addr)->s_addr = htonl(INADDR_ANY);
  ipv6_address_any.ip_ver = &ipv6;
  ipv6_address_any.addr = raw_allocate_on_heap(sizeof (struct in6_addr));
  *((struct in6_addr *) ipv6_address_any.addr) = in6addr_any;
}

struct ip_address *get_ipv4_address_any(){
  if(!IP_ADDRESS_ANY_INITIALIZED)
    initialize_ip_address_any();
  return (struct ip_address *) &ipv4_address_any;
}

struct ip_address *get_ipv6_address_any(){
  if(!IP_ADDRESS_ANY_INITIALIZED)
    initialize_ip_address_any();
  return (struct ip_address *) &ipv6_address_any;
}

int ip_ver_to_address_family (struct internal_ip_version *ip_ver){
  if (ip_ver == NULL){
    report_error_nil_access();
  }
  if(ip_ver->version == 1)
    return AF_INET;
  else if (ip_ver->version == 2)
    return AF_INET6;
  else{
    fprintf(stderr, "stdlib error: invalid ip version was used.\n");
    exit(1);
  }
}

size_t ip_ver_to_address_size (struct internal_ip_version *ip_ver){
  if (ip_ver == NULL){
    report_error_nil_access();
  }
  if(ip_ver->version == 1)
    return sizeof(struct in_addr);
  else if (ip_ver->version == 2)
    return sizeof(struct in6_addr);
  else{
    fprintf(stderr, "stdlib error: invalid ip version was used.\n");
    exit(1);
  }
}

size_t ip_ver_to_raw_socket_address_size (struct internal_ip_version *ip_ver){
  if (ip_ver == NULL){
    report_error_nil_access();
  }
  if(ip_ver->version == 1)
    return sizeof(struct sockaddr_in);
  else if (ip_ver->version == 2)
    return sizeof(struct sockaddr_in6);
  else{
    fprintf(stderr, "stdlib error: invalid ip version was used.\n");
    exit(1);
  }
}

int ctype_to_type (struct internal_connection_type *ctype){
  if (ctype == NULL){
    report_error_nil_access();
  }
  if(ctype->type == 1){
    return SOCK_STREAM;
  }
  else if (ctype->type == 2){
    return SOCK_DGRAM;
  }
  else{
    fprintf(stderr, "stdlib error: invalid connection type was used.\n");
    exit(1);
  }
}

int ctype_to_proto (struct internal_connection_type *ctype){
  if (ctype == NULL){
    report_error_nil_access();
  }
  if(ctype->type == 1){
    return IPPROTO_TCP;
  }
  else if (ctype->type == 2){
    return IPPROTO_UDP;
  }
  else{
    fprintf(stderr, "stdlib error: invalid connection type was used.\n");
    exit(1);
  }
}

struct socket *create_socket(struct ip_version *ip_ver, struct connection_type *ctype){
  int ipv = ip_ver_to_address_family((struct internal_ip_version *) ip_ver);
  int ctp = ctype_to_type((struct internal_connection_type *) ctype);
  int proto = ctype_to_proto((struct internal_connection_type *) ctype);
  int f = socket(ipv, ctp, proto);
  if (f == -1) return NULL;
  struct internal_socket *soc = raw_allocate_on_heap(sizeof(struct internal_socket));
  soc->socfd = f;
  soc->ip_ver = (struct internal_ip_version *) ip_ver;
  soc->ctype = (struct internal_connection_type *) ctype;
  soc->status = FRESH_SOCKET;
  soc->input_stream = NULL;
  soc->output_stream = NULL;
  return (struct socket *) soc;
}

struct stream *socket_get_input_stream(struct socket *soc){
  if (soc == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  if(isoc->status != CONNECTED_SOCKET && isoc->status != ACTIVE_UDP_SOCKET){
    perror("stdlib error: attempting to obtain inout stream of an unconnected/unactivated socket.");
    exit(1);
  }
  return isoc->input_stream;
}

struct stream *socket_get_output_stream(struct socket *soc){
  if (soc == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  if(isoc->status != CONNECTED_SOCKET && isoc->status != ACTIVE_UDP_SOCKET){
    perror("stdlib error: attempting to obtain output stream of an unconnected/unactivated socket.");
    exit(1);
  }
  return isoc->output_stream;
}

struct ip_address *string_to_ip_address(struct ip_version *ip_ver, struct array *addr){
  int ipv = ip_ver_to_address_family((struct internal_ip_version *) ip_ver);
  char *caddr = dolphin_string_to_c_string(addr);
  struct internal_ip_address *ipaddr = raw_allocate_on_heap(sizeof(struct internal_ip_address));
  ipaddr->ip_ver = (struct internal_ip_version *) ip_ver;
  ipaddr->addr = raw_allocate_on_heap(ip_ver_to_address_size((struct internal_ip_version *) ip_ver));
  int r = inet_pton(ipv, caddr, ipaddr->addr);
  if (r == 0 || r == -1)
    return NULL;
    return (struct ip_address *) ipaddr;
}

struct array *ip_address_to_string(struct ip_address *ipaddr){
  if (ipaddr == NULL){
    report_error_nil_access();
  }
  struct internal_ip_address *iipaddr = (struct internal_ip_address *)ipaddr;
  int allocated_len;
  int ipv = ip_ver_to_address_family(iipaddr->ip_ver);
  if(ipv == AF_INET){
    allocated_len = INET_ADDRSTRLEN;
  }
  else if(ipv == AF_INET6){
    allocated_len = INET6_ADDRSTRLEN;
  }
  char *caddrstr = raw_allocate_on_heap(allocated_len);
  if (inet_ntop(ipv, iipaddr->addr, caddrstr, allocated_len) == NULL) {
      perror("stdlib error: internal error; the function inet_ntop failed.");
      exit(1);
  }
  return c_string_to_dolphin_string(caddrstr);
}

struct socket_address *create_socket_address(struct ip_address *ipaddr, int64_t port){
  if (port < 0 || port > UINT16_MAX){
    perror("stdlib error: invalid port number was passed to the create_socket_address function.");
    exit(1);
  }
  if (ipaddr == NULL){
    report_error_nil_access();
  }
  struct internal_ip_address *iipaddr = (struct internal_ip_address *)ipaddr;
  int ipv = ip_ver_to_address_family(iipaddr->ip_ver);
  struct internal_socket_address *saddr = raw_allocate_on_heap(sizeof (struct internal_socket_address));
  saddr->ip_ver = iipaddr->ip_ver;
  saddr->raw_socket_address = raw_allocate_on_heap(ip_ver_to_raw_socket_address_size(iipaddr->ip_ver));
  if(ipv == AF_INET){
    struct sockaddr_in *addr_v4 = (struct sockaddr_in *) saddr->raw_socket_address;
    addr_v4->sin_addr = *((struct in_addr *) iipaddr->addr);
    addr_v4->sin_family = AF_INET;
    addr_v4->sin_port = htons(port);
    memset(addr_v4->sin_zero, 0, 8);
  }
  else if(ipv == AF_INET6){
    struct sockaddr_in6 *addr_v6 = (struct sockaddr_in6 *) saddr->raw_socket_address;
    addr_v6->sin6_family = AF_INET6;
    addr_v6->sin6_port = htons(port);
    addr_v6->sin6_addr = *((struct in6_addr *) iipaddr->addr);
    addr_v6->sin6_flowinfo = 0;
    addr_v6->sin6_scope_id = 0;
  }
  return (struct socket_address *) saddr;
}

struct ip_address *get_ip_address_of_socket_address(struct socket_address *saddr){
  if (saddr == NULL){
    report_error_nil_access();
  }
  struct internal_socket_address *isaddr = (struct internal_socket_address *) saddr;
  int ipv = ip_ver_to_address_family(isaddr->ip_ver);
  if(ipv == AF_INET){
    struct internal_ip_address *res = raw_allocate_on_heap(sizeof(struct internal_ip_address));
    res->ip_ver = isaddr->ip_ver;
    res->addr = raw_allocate_on_heap(ip_ver_to_address_size(isaddr->ip_ver));
    *((struct in_addr *) res->addr) = ((struct sockaddr_in *) isaddr->raw_socket_address)->sin_addr;
    return (struct ip_address *) res;
  }
  else if(ipv == AF_INET6){
    struct internal_ip_address *res = raw_allocate_on_heap(sizeof(struct internal_ip_address));
    res->ip_ver = isaddr->ip_ver;
    res->addr = raw_allocate_on_heap(ip_ver_to_address_size(isaddr->ip_ver));
    *((struct in6_addr *) res->addr) = ((struct sockaddr_in6 *) isaddr->raw_socket_address)->sin6_addr;
    return (struct ip_address *) res;
  }
  // should technically never happen.
  return NULL;
}

int64_t get_port_of_socket_address(struct socket_address *saddr){
  if (saddr == NULL){
    report_error_nil_access();
  }
  struct internal_socket_address *isaddr = (struct internal_socket_address *) saddr;
  int ipv = ip_ver_to_address_family(isaddr->ip_ver);
  if(ipv == AF_INET){
    return ((struct sockaddr_in *) isaddr->raw_socket_address)->sin_port;
  }
  else if(ipv == AF_INET6){
    return ((struct sockaddr_in6 *) isaddr->raw_socket_address)->sin6_port;
  }
  // should technically never happen.
  return 0;
}

// returns true if succeded and false otherwise.
char socket_bind(struct socket *soc, struct socket_address *saddr){
  if (soc == NULL || saddr == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  struct internal_socket_address *isaddr = (struct internal_socket_address *) saddr;
  if (isoc->status != FRESH_SOCKET){
    perror("stdlib error: attempting to bind a socket that is not fresh.");
    exit(1);
  }
  if (ip_ver_to_address_family (isoc->ip_ver) != ip_ver_to_address_family(isaddr->ip_ver)){
    perror("stdlib error: attempting to bind a socket to an incompatible address.");
    exit(1);
  }
  int r = bind(isoc->socfd, (struct sockaddr *) isaddr->raw_socket_address, ip_ver_to_raw_socket_address_size(isaddr->ip_ver));
  if (r == -1){
    return 0;
  }
  isoc->status = BOUND_SOCKET;
  return 1;
}

// returns true if succeded and false otherwise.
char socket_listen(struct socket *soc, int64_t backlog){
  int bclog = backlog > __INT_MAX__ ? __INT_MAX__ : backlog;
  if (soc == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  if(ctype_to_proto(isoc->ctype) != IPPROTO_TCP){
    perror("stdlib error: attempting to listen on a non-TCP socket.");
    exit(1);
  }
  if(isoc->status != BOUND_SOCKET)
  {
    perror("stdlib error: attempting to listen on a socket that is not in the freshly bound state.");
    exit(1);
  }
  if(ctype_to_proto(isoc->ctype) != IPPROTO_TCP)
  {
    perror("stdlib error: attempting to listen on a non-tcp socket.");
    exit(1);
  }
  int r = listen(isoc->socfd, bclog);
  if (r == -1){
    return 0;
  }
  isoc->status = LISTENING_SOCKET;
  return 1;
}

// returns the connection and the socket address of the other side which may be nil!
struct accepted_connection *socket_accept(struct socket * soc){
  if (soc == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  if(isoc->status != LISTENING_SOCKET)
  {
    perror("stdlib error: attempting to accept a connection on a socket that is not listening.");
    exit(1);
  }
  struct accepted_connection *accepted = raw_allocate_on_heap(sizeof (struct accepted_connection));
  accepted->other_side = raw_allocate_on_heap(sizeof (struct internal_socket_address));
  ((struct internal_socket_address *) accepted->other_side)->ip_ver = isoc->ip_ver;
  socklen_t len = sizeof (struct sockaddr_storage);
  ((struct internal_socket_address *) accepted->other_side)->raw_socket_address = raw_allocate_on_heap(len);
  int r = accept(isoc->socfd, ((struct internal_socket_address *) accepted->other_side)->raw_socket_address, &len);
  if(len != ip_ver_to_raw_socket_address_size(((struct internal_socket_address *) accepted->other_side)->ip_ver)){// there is some incompatibility; it is safer to return nil for the other side.
    free(((struct internal_socket_address *) accepted->other_side)->raw_socket_address);
    free(accepted->other_side);
    accepted->other_side = NULL;
  }
  if(r == -1){
    return NULL;
  }
  accepted->connection = raw_allocate_on_heap(sizeof (struct internal_socket));
  ((struct internal_socket *) accepted->connection)->ctype = isoc->ctype;
  ((struct internal_socket *) accepted->connection)->ip_ver = isoc->ip_ver;
  ((struct internal_socket *) accepted->connection)->socfd = r;
  ((struct internal_socket *) accepted->connection)->status = CONNECTED_SOCKET;
  int newfd_in = dup(((struct internal_socket *) accepted->connection)->socfd);
  if(newfd_in == -1)
  {
    perror("stdlib error: failed to create input stream for socket.");
    exit(1);
  }
  ((struct internal_socket *) accepted->connection)->input_stream = (struct stream *) fdopen(newfd_in, "rb");
  if(((struct internal_socket *) accepted->connection)->input_stream == NULL)
  {
    perror("stdlib error: failed to create input stream for socket.");
    exit(1);
  }
  int newfd_out = dup(((struct internal_socket *) accepted->connection)->socfd);
  if(newfd_out == -1)
  {
    perror("stdlib error: failed to create output stream for socket.");
    exit(1);
  }
  ((struct internal_socket *) accepted->connection)->output_stream = (struct stream *) fdopen(newfd_out, "wb+");
  if(((struct internal_socket *) accepted->connection)->output_stream == NULL)
  {
    perror("stdlib error: failed to create output stream for socket.");
    exit(1);
  }
  return accepted;
}

// returns true if succeded and false otherwise.
char socket_connect(struct socket *soc, struct socket_address *saddr){
  if (soc == NULL || saddr == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  struct internal_socket_address *isaddr = (struct internal_socket_address *) saddr;
  if(ctype_to_proto(isoc->ctype) != IPPROTO_TCP){
    perror("stdlib error: attempting to connect over a non-TCP socket.");
    exit(1);
  }
  if (isoc->status != FRESH_SOCKET){
    perror("stdlib error: attempting to connect over a socket that is not fresh.");
    exit(1);
  }
  if (ip_ver_to_address_family (isoc->ip_ver) != ip_ver_to_address_family(isaddr->ip_ver)){
    perror("stdlib error: attempting to connect over a socket to an incompatible address.");
    exit(1);
  }
  int r = connect(isoc->socfd, isaddr->raw_socket_address, ip_ver_to_raw_socket_address_size(isaddr->ip_ver));
  if (r == -1){
    return 0;
  }
  isoc->status = CONNECTED_SOCKET;
  int newfd_in = dup(isoc->socfd);
  if(newfd_in == -1)
  {
    perror("stdlib error: failed to create input stream for socket.");
    exit(1);
  }
  isoc->input_stream = (struct stream *) fdopen(newfd_in, "rb");
  if(isoc->input_stream == NULL)
  {
    perror("stdlib error: failed to create input stream for socket.");
    exit(1);
  }
  int newfd_out = dup(isoc->socfd);
  if(newfd_out == -1)
  {
    perror("stdlib error: failed to create output stream for socket.");
    exit(1);
  }
  isoc->output_stream = (struct stream *) fdopen(newfd_out, "wb+");
  if(isoc->output_stream == NULL)
  {
    perror("stdlib error: failed to create output stream for socket.");
    exit(1);
  }
  return 1;
}

// returns true if succeded and false otherwise; in practice always returns true!
char socket_activate_udp(struct socket *soc){
  if (soc == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  if(ctype_to_proto(isoc->ctype) != IPPROTO_UDP){
    perror("stdlib error: attempting to activate UDP communication nover a non-UDP socket.");
    exit(1);
  }
  if (isoc->status != BOUND_SOCKET){
    perror("stdlib error: attempting to activate UDP over an unbound socket.");
    exit(1);
  }
  isoc->status = ACTIVE_UDP_SOCKET;
  return 1;
}

// returns the number of bytes sent; returns a negative value (-1) in case of error.
int64_t socket_sendto_udp(struct socket *soc, struct socket_address *dest, struct array *data){
  if (soc == NULL || dest == NULL || data == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  struct internal_socket_address *idest = (struct internal_socket_address *) dest;
  if (ip_ver_to_address_family (isoc->ip_ver) != ip_ver_to_address_family(idest->ip_ver)){
    perror("stdlib error: attempting to send data over a socket with an incompatible address.");
    exit(1);
  }
  if (isoc->status != ACTIVE_UDP_SOCKET){
    perror("stdlib error: attempting to send data over an unactivated UDP socket.");
    exit(1);
  }
  return sendto(isoc->socfd, data->contents, data->len, 0, idest->raw_socket_address, ip_ver_to_raw_socket_address_size(idest->ip_ver));
}

char recvbuf[65507]; // the limit of what possibly can be received over udp.

struct udp_recvfrom_result *socket_recvfrom_udp(struct socket *soc){
  if (soc == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  if (isoc->status != ACTIVE_UDP_SOCKET){
    perror("stdlib error: attempting to receive data over an inactivated UDP socket.");
    exit(1);
  }
  struct udp_recvfrom_result *result = raw_allocate_on_heap(sizeof (struct udp_recvfrom_result));
  result->sender = raw_allocate_on_heap(sizeof (struct internal_socket_address));
  ((struct internal_socket_address *) result->sender)->ip_ver = isoc->ip_ver;
  socklen_t len = sizeof (struct sockaddr_storage);
  ((struct internal_socket_address *) result->sender)->raw_socket_address = raw_allocate_on_heap(len);
  size_t r = recvfrom(isoc->socfd, recvbuf, sizeof(recvbuf), 0, ((struct internal_socket_address *) result->sender)->raw_socket_address, &len);
  if(len != ip_ver_to_raw_socket_address_size(((struct internal_socket_address *) result->sender)->ip_ver)){// there is some incompatibility; it is safer to return nil for the other side.
    free((struct internal_socket_address *) result->sender);
    free(result->sender);
    result->sender = NULL;
  }
  if(r < 0){
    free(result); // in this case, the sender must also not be set correctly, and hence the socket address must already be freed.
    return NULL;
  }
  char tmp = 0;
  result->data = allocate_array(sizeof(char), r, &tmp);
  memcpy(result->data->contents, recvbuf, r);
  return result;
}

// returns true if succeded and false otherwise.
char socket_close(struct socket *soc){
  if (soc == NULL){
    report_error_nil_access();
  }
  struct internal_socket *isoc = (struct internal_socket *) soc;
  if (isoc->status == CONNECTED_SOCKET){
    if(fclose((FILE *) isoc->input_stream))
      return 0;
    if(fclose((FILE *) isoc->output_stream))
      return 0;
  }
  if(close(isoc->socfd))
    return 0;

  return 1;
}

int64_t read_integer () {
    int64_t value;
    printf("Please enter an integer: ");
    scanf("%" PRId64 "" , &value);
    return value;
}

void print_integer (int64_t value) {
    printf("%" PRId64 "\n" , value);
}