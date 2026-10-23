#include <libical/ical.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

/* libical caches builtin zones globally. Keep the pure parser/expander boundary
 * serialized; all returned strings are owned by the caller. */
static pthread_mutex_t calendar_lock = PTHREAD_MUTEX_INITIALIZER;
static icaltimezone *property_zone(icalcomponent *root, icalproperty *p) {
    icalparameter *parameter = icalproperty_get_first_parameter(p, ICAL_TZID_PARAMETER);
    if (!parameter) return NULL;
    const char *id = icalparameter_get_tzid(parameter);
    icaltimezone *zone = icalcomponent_get_timezone(root, id);
    if (!zone) zone = icaltimezone_get_builtin_timezone(id);
    return zone;
}
static int normalize_component(icalcomponent *root, icalcomponent *component) {
    for (icalproperty *p = icalcomponent_get_first_property(component, ICAL_ANY_PROPERTY); p; p = icalcomponent_get_next_property(component, ICAL_ANY_PROPERTY)) {
        icalvalue *value = icalproperty_get_value(p);
        if (!value) continue;
        if (icalproperty_isa(p) == ICAL_RDATE_PROPERTY && icalproperty_get_first_parameter(p,ICAL_TZID_PARAMETER)) {
            icaltimezone *zone = property_zone(root,p);
            if (!zone) return 0;
            struct icaldatetimeperiodtype date = icalproperty_get_rdate(p);
            struct icaltimetype *times[] = { &date.time, &date.period.start, &date.period.end };
            for (int i = 0; i < 3; i++) if (!icaltime_is_null_time(*times[i]) && !times[i]->is_date) {
                icaltime_set_timezone(times[i],zone);
                *times[i] = icaltime_convert_to_zone(*times[i],icaltimezone_get_utc_timezone());
            }
            icalproperty_set_rdate(p,date);
            continue;
        }
        if (icalvalue_isa(value) != ICAL_DATETIME_VALUE) continue;
        icalparameter *tzid = icalproperty_get_first_parameter(p, ICAL_TZID_PARAMETER);
        if (tzid) {
            icaltimezone *zone = property_zone(root,p);
            if (!zone) return 0;
            struct icaltimetype t = icalvalue_get_datetime(value);
            icaltime_set_timezone(&t,zone);
            t = icaltime_convert_to_zone(t, icaltimezone_get_utc_timezone());
            icalproperty_set_value(p, icalvalue_new_datetime(t));
        }
    }
    for (icalcomponent *c = icalcomponent_get_first_component(component, ICAL_ANY_COMPONENT); c; c = icalcomponent_get_next_component(component, ICAL_ANY_COMPONENT))
        if (icalcomponent_isa(c) != ICAL_VTIMEZONE_COMPONENT && !normalize_component(root,c)) return 0;
    return 1;
}
char *mates_calendar_normalize(const char *body) {
    pthread_mutex_lock(&calendar_lock);
    icalcomponent *root = icalparser_parse_string(body);
    char *result = NULL;
    if (root && icalcomponent_isa(root) == ICAL_VCALENDAR_COMPONENT && icalcomponent_count_errors(root) == 0 && normalize_component(root,root))
        result = icalcomponent_as_ical_string_r(root);
    if (root) icalcomponent_free(root);
    pthread_mutex_unlock(&calendar_lock);
    return result;
}
static time_t instant(struct icaltimetype t) {
    return icaltime_as_timet_with_zone(t, icaltime_get_timezone(t) ? icaltime_get_timezone(t) : icaltimezone_get_utc_timezone());
}
static int append_epoch(char **buffer, size_t *length, time_t value) {
    char line[64];
    int count = snprintf(line,sizeof(line),"%lld\n",(long long)value);
    char *next = realloc(*buffer,*length + count + 1);
    if (!next) return 0;
    *buffer = next; memcpy(next + *length,line,count + 1); *length += count;
    return 1;
}
/* Returns an epoch list, or NULL on invalid rules, unknown zones, or a bounded
 * expansion limit. Haskell turns this into a typed protocol error. */
char *mates_calendar_occurrences(const char *body, const char *uid, const char *kind,
                                int64_t window_start, int64_t window_end) {
    pthread_mutex_lock(&calendar_lock);
    icalcomponent *root = icalparser_parse_string(body);
    char *result = calloc(1,1); size_t length = 0; int work = 0; int ok = root && result;
    icalcomponent_kind wanted = !strcmp(kind,"VTODO") ? ICAL_VTODO_COMPONENT : ICAL_VEVENT_COMPONENT;
    if (root) for (icalcomponent *c = icalcomponent_get_first_component(root,wanted); c; c = icalcomponent_get_next_component(root,wanted)) {
        const char *component_uid = icalcomponent_get_uid(c);
        if (component_uid && strcmp(component_uid,uid)) continue;
        if (icalcomponent_get_first_property(c,ICAL_RECURRENCEID_PROPERTY)) continue;
        icalproperty *start_property = icalcomponent_get_first_property(c,ICAL_DTSTART_PROPERTY);
        if (!start_property && wanted == ICAL_VTODO_COMPONENT) start_property = icalcomponent_get_first_property(c,ICAL_DUE_PROPERTY);
        if (!start_property) continue;
        struct icaltimetype start = icalproperty_get_datetime_with_component(start_property,c);
        if (icalproperty_get_first_parameter(start_property,ICAL_TZID_PARAMETER) && !property_zone(root,start_property)) { ok = 0; break; }
        icaltimezone *zone = property_zone(root,start_property);
        if (zone) icaltime_set_timezone(&start,zone);
        for (icalproperty *p = icalcomponent_get_first_property(c,ICAL_RRULE_PROPERTY); p && ok; p = icalcomponent_get_next_property(c,ICAL_RRULE_PROPERTY)) {
            struct icalrecurrencetype rule = icalproperty_get_rrule(p);
            icalrecur_iterator *iterator = icalrecur_iterator_new(rule,start);
            if (!iterator) { ok = 0; break; }
            if (!rule.count && window_start > instant(start)) {
                struct icaltimetype seek = icaltime_from_timet_with_zone((time_t)window_start, start.is_date, icaltime_get_timezone(start));
                icalrecur_iterator_set_start(iterator,seek);
            }
            for (;;) {
                struct icaltimetype next = icalrecur_iterator_next(iterator);
                if (icaltime_is_null_time(next)) break;
                if (++work > 100000) { ok = 0; break; }
                time_t epoch = instant(next);
                if (epoch >= window_end) break;
                if (epoch >= window_start && !icalproperty_recurrence_is_excluded(c,&start,&next) && !append_epoch(&result,&length,epoch)) { ok = 0; break; }
            }
            icalrecur_iterator_free(iterator);
        }
    }
    if (root) icalcomponent_free(root);
    if (!ok) { free(result); result = NULL; }
    pthread_mutex_unlock(&calendar_lock);
    return result;
}

char *mates_calendar_timezone(const char *id) {
    pthread_mutex_lock(&calendar_lock);
    char *result = NULL;
    if (!strcmp(id,"UTC")) {
        result = strdup("BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTIMEZONE\r\nTZID:UTC\r\nBEGIN:STANDARD\r\nDTSTART:19700101T000000\r\nTZOFFSETFROM:+0000\r\nTZOFFSETTO:+0000\r\nEND:STANDARD\r\nEND:VTIMEZONE\r\nEND:VCALENDAR\r\n");
    } else {
        icaltimezone *zone = icaltimezone_get_builtin_timezone(id);
        icalcomponent *definition = zone ? icaltimezone_get_component(zone) : NULL;
        if (definition) {
            icalcomponent *calendar = icalcomponent_new(ICAL_VCALENDAR_COMPONENT);
            icalcomponent_add_property(calendar,icalproperty_new_version("2.0"));
            icalcomponent *copy = icalcomponent_new_clone(definition);
            /* Return the configured IANA identifier rather than libical's
             * internal location-qualified identifier. */
            icalproperty *tzid = icalcomponent_get_first_property(copy,ICAL_TZID_PROPERTY);
            if (tzid) icalproperty_set_tzid(tzid,id);
            icalcomponent_add_component(calendar,copy);
            result = icalcomponent_as_ical_string_r(calendar);
            icalcomponent_free(calendar);
        }
    }
    pthread_mutex_unlock(&calendar_lock);
    return result;
}
