#include <curl/curl.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

struct buffer { char *data; size_t size; };
static pthread_once_t curl_once = PTHREAD_ONCE_INIT;
static void initialize_curl(void) { curl_global_init(CURL_GLOBAL_DEFAULT); }
static size_t receive_bytes(char *data, size_t size, size_t count, void *context) {
    struct buffer *b = context;
    if (count && size > SIZE_MAX / count) return 0;
    size_t bytes = size * count;
    if (bytes > 64 * 1024 * 1024 || b->size > 64 * 1024 * 1024 - bytes) return 0;
    char *next = realloc(b->data, b->size + bytes + 1);
    if (!next) return 0;
    b->data = next;
    memcpy(next + b->size, data, bytes);
    b->size += bytes; next[b->size] = 0;
    return bytes;
}
int mates_http(const char *url, const char *user, const char *password,
               const char *method, const char *depth, const char *body, size_t length,
               char **result, size_t *result_length, long *status, char *error) {
    pthread_once(&curl_once, initialize_curl);
    CURL *curl = curl_easy_init();
    if (!curl) { strcpy(error, "Cannot initialize libcurl"); return 1; }
    struct buffer output = {NULL, 0};
    struct curl_slist *headers = NULL;
    char depth_header[64];
    snprintf(depth_header, sizeof(depth_header), "Depth: %s", depth);
    headers = curl_slist_append(headers, depth_header);
    headers = curl_slist_append(headers, "Accept: application/xml, text/xml");
    headers = curl_slist_append(headers, "Content-Type: application/xml; charset=utf-8");
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_USERNAME, user);
    curl_easy_setopt(curl, CURLOPT_PASSWORD, password);
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long) CURLAUTH_ANY);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, method);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE_LARGE, (curl_off_t) length);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_MAXREDIRS, 10L);
    curl_easy_setopt(curl, CURLOPT_PROTOCOLS_STR, "http,https");
    curl_easy_setopt(curl, CURLOPT_REDIR_PROTOCOLS_STR, strncmp(url, "https:", 6) ? "http,https" : "https");
    curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 15L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 120L);
    curl_easy_setopt(curl, CURLOPT_NOSIGNAL, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, receive_bytes);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &output);
    curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, error);
    CURLcode code = curl_easy_perform(curl);
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, status);
    if (code && !*error) snprintf(error, CURL_ERROR_SIZE, "%s", curl_easy_strerror(code));
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    *result = output.data; *result_length = output.size;
    return (int) code;
}
