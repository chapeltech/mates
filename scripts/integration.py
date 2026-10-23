#!/usr/bin/env python3
"""Real HTTP/nginx/vdirsyncer checks against a disposable PostgreSQL database.

Run after cabal build, with PGHOST/PGPORT selecting a development PostgreSQL.
No existing database is reset. The temporary database and processes are removed.
"""
import base64
import concurrent.futures
import hashlib
import http.client
import os
from pathlib import Path
import shutil
import socket
import subprocess
import tempfile
import time
import unittest
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
NS = {"d": "DAV:", "c": "urn:ietf:params:xml:ns:carddav", "cal": "urn:ietf:params:xml:ns:caldav"}


def port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def card(uid, name="Ada", extra=""):
    return f"BEGIN:VCARD\r\nVERSION:4.0\r\nUID:{uid}\r\nFN:{name}\r\n{extra}END:VCARD\r\n".encode()


def event(uid, details):
    return f"BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//mates tests//EN\r\nBEGIN:VEVENT\r\nUID:{uid}\r\n{details}END:VEVENT\r\nEND:VCALENDAR\r\n".encode()


class Integration(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory(prefix="mates-http-")
        cls.addClassCleanup(cls.cleanup)
        cls.directory = Path(cls.temp.name)
        cls.binary = subprocess.check_output(["cabal", "list-bin", "exe:mates", "--enable-optimization=2"], cwd=ROOT, text=True).strip()
        cls.database = f"mates_http_{os.getpid()}_test"
        subprocess.run(["createdb", cls.database], check=True)
        cls.env = dict(os.environ, MATES_DATABASE_URL=f"dbname={cls.database}", MATES_SCHEMA_PATH=str(ROOT / "db/schema.sql"))
        cls.backend_port, cls.proxy_port = port(), port()
        cls.backend_log = (cls.directory / "backend.log").open("w")
        cls.start_backend()
        for username in ("alice", "reader", "calendar", "disabled"):
            cls.cli("principal", "add", username)
        cls.cli("principal", "disable", "disabled")
        for name in ("personal", "private"):
            cls.cli("collection", "create", f"/dav/calendars/{name}/", name)
        cls.cli("access", "grant", "alice", "/dav/contacts/", "write")
        cls.cli("access", "grant", "reader", "/dav/contacts/", "read")
        cls.cli("access", "grant", "alice", "/dav/calendars/personal/", "write")
        cls.cli("access", "grant", "calendar", "/dav/calendars/private/", "write")
        password = base64.b64encode(hashlib.sha1(b"secret").digest()).decode()
        (cls.directory / "users").write_text(f"alice:{{SHA}}{password}\nreader:{{SHA}}{password}\n")
        (cls.directory / "nginx.conf").write_text(f"""pid {cls.directory}/nginx.pid;
error_log {cls.directory}/nginx-error.log;
events {{ worker_connections 64; }}
http {{ fastcgi_temp_path {cls.directory}/fastcgi; uwsgi_temp_path {cls.directory}/uwsgi; scgi_temp_path {cls.directory}/scgi; client_body_temp_path {cls.directory}/client_body; proxy_temp_path {cls.directory}/proxy_temp; access_log off; server {{ listen 127.0.0.1:{cls.proxy_port};
 location / {{ auth_basic "mates"; auth_basic_user_file {cls.directory}/users;
 proxy_set_header X-REMOTE-USER $remote_user;
 proxy_pass http://127.0.0.1:{cls.backend_port}; }} }} }}
""")
        cls.proxy = subprocess.Popen(["nginx", "-e", str(cls.directory / "nginx-start.log"), "-p", str(cls.directory), "-c", str(cls.directory / "nginx.conf"), "-g", "daemon off;"], stdout=subprocess.DEVNULL, stderr=cls.backend_log)
        cls.wait(cls.proxy_port)

    @classmethod
    def wait(cls, service_port):
        for _ in range(200):
            try:
                with socket.create_connection(("127.0.0.1", service_port), timeout=.1):
                    return
            except OSError:
                time.sleep(.025)
        raise RuntimeError((cls.directory / "backend.log").read_text())

    @classmethod
    def start_backend(cls):
        cls.backend = subprocess.Popen([cls.binary, "serve", "--port", str(cls.backend_port), "--public-url", f"http://127.0.0.1:{cls.proxy_port}"], cwd=ROOT, env=cls.env, stdout=cls.backend_log, stderr=cls.backend_log)
        cls.wait(cls.backend_port)

    @classmethod
    def cli(cls, *args):
        return subprocess.check_output([cls.binary, *args], cwd=ROOT, env=cls.env, text=True)

    @classmethod
    def cleanup(cls):
        for process in (getattr(cls, "proxy", None), getattr(cls, "backend", None)):
            if process:
                process.terminate()
                process.wait(timeout=10)
        cls.backend_log.close()
        shutil.copyfile(cls.directory / "backend.log", "/tmp/mates-integration-backend.log")
        subprocess.run(["dropdb", "--force", cls.database], check=True)
        cls.temp.cleanup()

    def request(self, method, href, body=b"", user="alice", headers=None, proxy=False):
        connection = http.client.HTTPConnection("127.0.0.1", self.proxy_port if proxy else self.backend_port, timeout=15)
        values = dict(headers or {})
        if user and "X-REMOTE-USER" not in values:
            values["X-REMOTE-USER"] = user
        if proxy and user:
            values["Authorization"] = "Basic " + base64.b64encode(f"{user}:secret".encode()).decode()
        connection.request(method, href, body, values)
        response = connection.getresponse()
        result = response.status, dict(response.getheaders()), response.read()
        connection.close()
        return result

    def propfind(self, href, user="alice", body=None, **kwargs):
        if body is None:
            body = '<x:propfind xmlns:x="DAV:"><x:prop><x:resourcetype/><x:sync-token/><x:current-user-principal/><x:current-user-privilege-set/></x:prop></x:propfind>'
        return self.request("PROPFIND", href, body.encode(), user, {"Depth": "1"}, **kwargs)

    def sync(self, href, token="", limit=None):
        limited = f"<x:limit><x:nresults>{limit}</x:nresults></x:limit>" if limit else ""
        body = f'<x:sync-collection xmlns:x="DAV:"><x:sync-token>{token}</x:sync-token><x:sync-level>1</x:sync-level>{limited}<x:prop><x:getetag/></x:prop></x:sync-collection>'
        return self.request("REPORT", href, body.encode())

    def query(self, href, filter_xml="", calendar=False, data=""):
        namespace = NS["cal"] if calendar else NS["c"]
        report = "calendar-query" if calendar else "addressbook-query"
        prop = "calendar-data" if calendar else "address-data"
        body = f'<q:{report} xmlns:q="{namespace}" xmlns:z="DAV:"><z:prop><z:getetag/><q:{prop}>{data}</q:{prop}></z:prop>{filter_xml}</q:{report}>'
        return self.request("REPORT", href, body.encode())

    def test_authentication_and_proxy_header(self):
        self.assertEqual(200, self.request("GET", "/health", user=None)[0])
        for user, status in ((None, 401), ("unknown", 403), ("disabled", 403)):
            self.assertEqual(status, self.request("GET", "/ui/", user=user)[0])
        self.assertEqual(401, self.propfind("/dav/", user=None, proxy=True)[0])
        self.assertEqual(207, self.propfind("/dav/", proxy=True)[0])
        connection = http.client.HTTPConnection("127.0.0.1", self.backend_port)
        connection.putrequest("GET", "/ui/")
        connection.putheader("X-REMOTE-USER", "alice")
        connection.putheader("X-REMOTE-USER", "reader")
        connection.endheaders()
        response = connection.getresponse()
        self.assertEqual(401, response.status)
        response.read(); connection.close()
        # A client-supplied identity is overwritten by nginx's authenticated user.
        status, _, _ = self.request("PUT", "/dav/contacts/proxy.vcf", card("proxy"), "alice", {"X-REMOTE-USER": "reader"}, proxy=True)
        self.assertEqual(201, status)

    def test_permissions_and_multiget_scope(self):
        self.assertEqual(403, self.request("PUT", "/dav/contacts/read-only.vcf", card("read-only"), "reader")[0])
        status, _, body = self.propfind("/dav/calendars/")
        self.assertEqual(207, status)
        self.assertIn(b"personal", body)
        self.assertNotIn(b"private", body)
        self.assertEqual(403, self.request("GET", "/dav/calendars/private/hidden.ics")[0])
        body = b'<C:calendar-multiget xmlns:C="urn:ietf:params:xml:ns:caldav" xmlns:D="DAV:"><D:prop><D:getetag/><C:calendar-data/></D:prop><D:href>/dav/contacts/proxy.vcf</D:href></C:calendar-multiget>'
        status, _, result = self.request("REPORT", "/dav/calendars/personal/", body)
        self.assertEqual(207, status)
        self.assertIn(b"404 Not Found", result)
        self.assertNotIn(b"BEGIN:VCARD", result)
        self.assertEqual(403, self.request("PUT", "/dav/calendars/unprovisioned/x.ics", event("x", "DTSTART:20261022T120000Z\r\n"))[0])

    def test_xml_properties_and_filters(self):
        raw = card("xml", "Renée & Ada", "EMAIL;TYPE=HOME:ada@example.test\r\nX-UNCHANGED;X-LABEL=\"a;b\":opaque\r\n")
        self.assertEqual(201, self.request("PUT", "/dav/contacts/xml.vcf", raw)[0])
        selected = '<propfind xmlns="DAV:" xmlns:U="urn:unknown"><prop><getetag/><U:missing/></prop></propfind>'
        status, _, body = self.propfind("/dav/contacts/xml.vcf", body=selected)
        self.assertEqual(207, status)
        self.assertIn(b"404 Not Found", body)
        self.assertNotIn(b"getcontentlength", body)
        filter_xml = '<q:filter test="allof"><q:prop-filter name="FN"><q:text-match match-type="contains">Renée &amp; Ada</q:text-match></q:prop-filter><q:prop-filter name="EMAIL"><q:param-filter name="TYPE"><q:text-match>HOME</q:text-match></q:param-filter></q:prop-filter></q:filter>'
        status, _, body = self.query("/dav/contacts/", filter_xml, data='<q:prop name="FN"/>')
        self.assertEqual(207, status)
        root = ET.fromstring(body)
        data = root.find(".//c:address-data", NS).text
        self.assertIn("FN:Renée & Ada", data)
        self.assertNotIn("EMAIL", data)
        escaped_href = "/dav/contacts/a%20name.vcf"
        self.assertEqual(201, self.request("PUT", escaped_href, card("escaped-href"))[0])
        multiget = b'<C:addressbook-multiget xmlns:C="urn:ietf:params:xml:ns:carddav" xmlns:D="DAV:"><D:prop><C:address-data/></D:prop><D:href>/dav/contacts/a%20name.vcf</D:href></C:addressbook-multiget>'
        self.assertIn(b"UID:escaped-href", self.request("REPORT", "/dav/contacts/", multiget)[2])
        multiget = multiget.replace(b"/dav/contacts/a%20name.vcf", b"https://foreign.invalid/dav/contacts/a%20name.vcf")
        self.assertNotIn(b"UID:escaped-href", self.request("REPORT", "/dav/contacts/", multiget)[2])
        decomposed = '<q:filter><q:prop-filter name="FN"><q:text-match match-type="equals">Rene\u0301e &amp; Ada</q:text-match></q:prop-filter></q:filter>'
        self.assertIn(b"xml.vcf", self.query("/dav/contacts/", decomposed)[2])
        anyof = '<q:filter><q:prop-filter name="FN"><q:text-match>Nobody</q:text-match></q:prop-filter><q:prop-filter name="EMAIL"><q:text-match>ada@example.test</q:text-match></q:prop-filter></q:filter>'
        self.assertIn(b"xml.vcf", self.query("/dav/contacts/", anyof)[2])
        self.assertEqual(400, self.request("REPORT", "/dav/contacts/", b"<broken>")[0])
        self.assertEqual(400, self.request("REPORT", "/dav/contacts/", b'<!DOCTYPE x [<!ENTITY e SYSTEM "file:///etc/passwd">]><x/>')[0])
        bad = '<q:filter><q:unsupported/></q:filter>'
        self.assertEqual(403, self.query("/dav/contacts/", bad)[0])
        self.assertEqual(413, self.request("PUT", "/dav/contacts/large.vcf", b"a" * (10485760 + 1))[0])

    def test_conditional_writes_history_and_restart(self):
        href = "/dav/contacts/concurrent.vcf"
        raw = card("concurrent", "Before", "X-RAW:untouched\r\n")
        self.assertEqual(201, self.request("PUT", href, raw)[0])
        status, headers, body = self.request("GET", href)
        tag = headers["ETag"]
        self.assertEqual(64, len(tag.strip('"')))
        self.assertEqual(304, self.request("GET", href, headers={"If-None-Match": tag})[0])
        self.assertEqual(tag, self.request("HEAD", href)[1]["ETag"])
        with concurrent.futures.ThreadPoolExecutor(2) as pool:
            futures = [pool.submit(self.request, "PUT", href, card("concurrent", name), "alice", {"If-Match": tag}) for name in ("First", "Second")]
            self.assertEqual([204, 412], sorted(f.result()[0] for f in futures))
        query = "SELECT encode(r.raw_payload, 'hex'), actor_principal FROM dav_object_revisions r JOIN dav_objects o ON o.id = r.dav_object_id WHERE o.href = '/dav/contacts/concurrent.vcf' ORDER BY revision_no LIMIT 1"
        row = subprocess.check_output(["psql", self.database, "-At", "-c", query], text=True).strip()
        self.assertEqual(raw.hex() + "|alice", row)
        before = ET.fromstring(self.sync("/dav/contacts/")[2]).find("d:sync-token", NS).text
        self.backend.terminate(); self.backend.wait(timeout=10)
        type(self).start_backend()
        result = self.sync("/dav/contacts/", before)
        self.assertEqual(207, result[0])
        self.assertEqual(before, ET.fromstring(result[2]).find("d:sync-token", NS).text)
        self.assertEqual(403, self.sync("/dav/calendars/personal/", before)[0])
        self.assertEqual(204, self.request("DELETE", href)[0])
        delta = ET.fromstring(self.sync("/dav/contacts/", before)[2])
        self.assertTrue(any(r.find("d:href", NS).text == href and "404" in r.find("d:status", NS).text for r in delta.findall("d:response", NS)))

    def test_calendar_timezone_recurrence_and_projection(self):
        details = "DTSTART;TZID=Europe/London:20261025T090000\r\nDTEND;TZID=Europe/London:20261025T100000\r\nRRULE:FREQ=MONTHLY;BYDAY=SU;BYSETPOS=-1;COUNT=3\r\nSUMMARY:Recurring\r\nATTENDEE;CN=Alice;PARTSTAT=ACCEPTED:mailto:alice@example.test\r\nBEGIN:VALARM\r\nACTION:DISPLAY\r\nTRIGGER:-PT15M\r\nDESCRIPTION:Reminder\r\nEND:VALARM\r\n"
        self.assertEqual(201, self.request("PUT", "/dav/calendars/personal/recur.ics", event("recur", details))[0])
        filter_xml = '<q:filter><q:comp-filter name="VCALENDAR"><q:comp-filter name="VEVENT"><q:time-range start="20261129T093000Z" end="20261129T094500Z"/></q:comp-filter></q:comp-filter></q:filter>'
        status, _, body = self.query("/dav/calendars/personal/", filter_xml, calendar=True)
        self.assertEqual(207, status)
        self.assertIn(b"recur.ics", body)
        miss = filter_xml.replace("20261129", "20261122")
        self.assertNotIn(b"recur.ics", self.query("/dav/calendars/personal/", miss, calendar=True)[2])
        query = "SELECT (SELECT count(*) FROM calendar_participants), (SELECT count(*) FROM calendar_alarms), (SELECT e.dtstart_tzid FROM calendar_events e JOIN calendar_components c ON c.id = e.component_id WHERE c.ical_uid = 'recur')"
        row = subprocess.check_output(["psql", self.database, "-At", "-c", query], text=True).strip()
        self.assertEqual("1|1|Europe/London", row)
        floating = event("floating", "DTSTART:20261022T100000\r\nSUMMARY:Floating\r\n")
        self.assertEqual(201, self.request("PUT", "/dav/calendars/personal/floating.ics", floating)[0])
        query = "SELECT dtstart_at IS NULL AND dtstart_local IS NOT NULL FROM calendar_events e JOIN calendar_components c ON c.id = e.component_id WHERE c.ical_uid = 'floating'"
        self.assertEqual("t", subprocess.check_output(["psql", self.database, "-At", "-c", query], text=True).strip())

    def test_explicit_relationship_and_derived_sync(self):
        for uid in ("target1", "target2"):
            self.assertEqual(201, self.request("PUT", f"/dav/contacts/{uid}.vcf", card(uid, "Duplicate"))[0])
        target = f"http://127.0.0.1:{self.proxy_port}/dav/contacts/target2.vcf"
        self.assertEqual(201, self.request("PUT", "/dav/contacts/source.vcf", card("source", "Source", f"RELATED;TYPE=mother;VALUE=uri:{target}\r\n"))[0])
        body = self.request("GET", "/dav/contacts/source.vcf")[2]
        self.assertIn(target.encode(), body.replace(b"\r\n ", b""))
        token = ET.fromstring(self.sync("/dav/contacts/")[2]).find("d:sync-token", NS).text
        self.assertEqual(204, self.request("PUT", "/dav/contacts/source.vcf", card("source", "Renamed source", f"RELATED;TYPE=mother;VALUE=uri:{target}\r\n"))[0])
        delta = self.sync("/dav/contacts/", token)[2]
        self.assertIn(b"target2.vcf", delta)
        self.assertNotIn(b"target1.vcf", delta)
        apple = self.request("GET", "/dav/contacts/source.vcf", headers={"User-Agent": "Apple Contacts"})[2]
        self.assertIn(b"X-ABRELATEDNAMES:Duplicate", apple)
        self.assertIn(target.encode(), apple.replace(b"\r\n ", b""))
        self.assertEqual(204, self.request("PUT", "/dav/contacts/source.vcf", apple)[0])
        # The explicit target survives an Apple-style round trip and a rename.
        self.assertEqual(204, self.request("PUT", "/dav/contacts/target2.vcf", card("target2", "Renamed target"))[0])
        self.assertIn(target.encode(), self.request("GET", "/dav/contacts/source.vcf")[2].replace(b"\r\n ", b""))

    def test_sync_pagination_revocation_and_webdav_if(self):
        for index in range(4):
            self.assertEqual(201, self.request("PUT", f"/dav/contacts/page{index}.vcf", card(f"page{index}"))[0])
        seen = set()
        token = ""
        for _ in range(100):
            status, _, body = self.sync("/dav/contacts/", token, 2)
            self.assertEqual(207, status)
            root = ET.fromstring(body)
            token = root.find("d:sync-token", NS).text
            truncated = False
            for resource in root.findall("d:response", NS):
                href = resource.find("d:href", NS).text
                status_node = resource.find("d:status", NS)
                if status_node is not None and "507" in status_node.text:
                    truncated = True
                else:
                    seen.add(href)
            if not truncated:
                break
        else:
            self.fail("Sync pagination did not terminate")
        self.assertTrue({f"/dav/contacts/page{i}.vcf" for i in range(4)} <= seen)
        condition = f'</dav/contacts/> (<{token}>)'
        self.assertEqual(201, self.request("PUT", "/dav/contacts/if.vcf", card("if"), headers={"If": condition})[0])
        self.assertEqual(412, self.request("PUT", "/dav/contacts/if.vcf", card("if", "Changed"), headers={"If": condition})[0])
        self.cli("access", "revoke", "reader", "/dav/contacts/")
        self.assertEqual(403, self.request("GET", "/dav/contacts/if.vcf", user="reader")[0])
        self.cli("access", "grant", "reader", "/dav/contacts/", "read")
        self.assertEqual(200, self.request("GET", "/dav/contacts/if.vcf", user="reader")[0])

    def test_calendar_dst_periods_selection_and_limits(self):
        href = "/dav/calendars/personal/dst.ics"
        details = "DTSTART;TZID=Europe/London:20261024T090000\r\nDTEND;TZID=Europe/London:20261024T100000\r\nRRULE:FREQ=DAILY;COUNT=3\r\nSUMMARY:DST appointment\r\nDESCRIPTION:Not selected\r\n"
        self.assertEqual(201, self.request("PUT", href, event("dst", details))[0])
        def matching(start, end, data=""):
            filter_xml = f'<q:filter><q:comp-filter name="VCALENDAR"><q:comp-filter name="VEVENT"><q:time-range start="{start}" end="{end}"/></q:comp-filter></q:comp-filter></q:filter>'
            return self.query("/dav/calendars/personal/", filter_xml, calendar=True, data=data)
        self.assertIn(b"dst.ics", matching("20261025T093000Z", "20261025T094500Z")[2])
        self.assertNotIn(b"dst.ics", matching("20261025T083000Z", "20261025T084500Z")[2])
        selection = '<q:comp name="VCALENDAR"><q:comp name="VEVENT"><q:prop name="SUMMARY"/></q:comp></q:comp>'
        result = matching("20261025T093000Z", "20261025T094500Z", selection)
        values = [e.text for e in ET.fromstring(result[2]).findall(".//cal:calendar-data", NS)]
        self.assertTrue(any("SUMMARY:DST appointment" in v for v in values))
        self.assertTrue(all("DESCRIPTION:" not in v for v in values))
        period = event("period", "DTSTART:20261020T100000Z\r\nDURATION:PT1H\r\nRDATE;VALUE=PERIOD:20261026T100000Z/PT4H\r\n")
        self.assertEqual(201, self.request("PUT", "/dav/calendars/personal/period.ics", period)[0])
        self.assertIn(b"period.ics", matching("20261026T130000Z", "20261026T133000Z")[2])
        self.assertNotIn(b"period.ics", matching("20261026T143000Z", "20261026T144500Z")[2])
        dense = event("dense", "DTSTART:20200101T000000Z\r\nRRULE:FREQ=SECONDLY;COUNT=200000\r\n")
        self.assertEqual(201, self.request("PUT", "/dav/calendars/personal/dense.ics", dense)[0])
        result = matching("20261026T130000Z", "20261026T133000Z")
        self.assertEqual(403, result[0])
        self.assertIn(b"number-of-matches-within-limits", result[2])
        self.assertEqual(204, self.request("DELETE", "/dav/calendars/personal/dense.ics")[0])

    def test_collection_metadata_and_floating_timezone(self):
        href = "/dav/calendars/named/"
        self.cli("collection", "create", href, "A named calendar")
        self.cli("collection", "timezone", href, "Europe/London")
        self.cli("access", "grant", "alice", href, "write")
        raw = event("named", "DTSTART:20261022T100000\r\nDURATION:PT1H\r\nSUMMARY:Floating in London\r\n")
        self.assertEqual(201, self.request("PUT", href + "named.ics", raw)[0])
        propfind = '<D:propfind xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:prop><D:displayname/><C:calendar-timezone/></D:prop></D:propfind>'
        status, _, body = self.propfind(href, body=propfind)
        root = ET.fromstring(body)
        self.assertEqual("A named calendar", root.find(".//d:displayname", NS).text)
        self.assertIn("TZID:Europe/London", root.find(".//cal:calendar-timezone", NS).text)
        filter_xml = '<q:filter><q:comp-filter name="VCALENDAR"><q:comp-filter name="VEVENT"><q:time-range start="20261022T093000Z" end="20261022T094500Z"/></q:comp-filter></q:comp-filter></q:filter>'
        self.assertIn(b"named.ics", self.query(href, filter_xml, calendar=True)[2])
        utc = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTIMEZONE\r\nTZID:UTC\r\nBEGIN:STANDARD\r\nDTSTART:19700101T000000\r\nTZOFFSETFROM:+0000\r\nTZOFFSETTO:+0000\r\nEND:STANDARD\r\nEND:VTIMEZONE\r\nEND:VCALENDAR"
        self.assertNotIn(b"named.ics", self.query(href, filter_xml + "<q:timezone>" + utc + "</q:timezone>", calendar=True)[2])
        self.assertEqual(raw, self.request("GET", href + "named.ics")[2])

    def test_projection_rebuild_preserves_raw_and_relationship_metadata(self):
        raw = card("metadata", " Metadata ", "RELATED;TYPE=friend;VALUE=text;PREF=1:Unresolved Friend\r\nNOTE: leading and trailing spaces \r\nX-CUSTOM;X-PARAM=\"a;b\":value\r\nPHOTO:data:image/png;base64,YWJj\r\n")
        self.assertEqual(201, self.request("PUT", "/dav/contacts/metadata.vcf", raw)[0])
        self.assertEqual(raw, self.request("GET", "/dav/contacts/metadata.vcf")[2])
        query = "UPDATE dav_object_projections SET parser_version = 'stale'; SELECT count(*) FROM dav_object_revisions"
        before = subprocess.check_output(["psql", self.database, "-At", "-c", query], text=True).strip().splitlines()[-1]
        self.backend.terminate(); self.backend.wait(timeout=10)
        type(self).start_backend()
        self.assertEqual(raw, self.request("GET", "/dav/contacts/metadata.vcf")[2])
        after = subprocess.check_output(["psql", self.database, "-At", "-c", "SELECT count(*) FROM dav_object_revisions"], text=True).strip()
        self.assertEqual(before, after)
        forbidden = subprocess.run(["psql", self.database, "-At", "-c", "UPDATE dav_object_revisions SET raw_payload = raw_payload"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.assertNotEqual(0, forbidden.returncode)
        self.assertIn(b"append-only", forbidden.stderr)
        binary = subprocess.check_output(["psql", self.database, "-At", "-c", "SELECT encode(m.data, 'hex') FROM contact_media m JOIN contacts c ON c.id = m.contact_id WHERE c.vcard_uid = 'metadata'"], text=True).strip()
        self.assertEqual("616263", binary)

    def test_principal_discovery_and_uid_conflicts(self):
        status, _, body = self.propfind("/dav/")
        principal = ET.fromstring(body).find(".//d:current-user-principal/d:href", NS).text
        self.assertEqual(207, self.propfind(principal)[0])
        self.assertEqual(308, self.request("GET", "/.well-known/carddav", user=None)[0])
        self.assertEqual(201, self.request("PUT", "/dav/contacts/unique.vcf", card("unique"))[0])
        self.assertIn(self.request("PUT", "/dav/contacts/duplicate-uid.vcf", card("unique"))[0], (403, 409))
        self.assertIn(self.request("PUT", "/dav/contacts/unique.vcf", card("different-uid"))[0], (403, 409))
        self.assertIn(b"UID:unique", self.request("GET", "/dav/contacts/unique.vcf")[2])
        self.assertEqual(201, self.request("PUT", "/dav/contacts/concurrent-create.vcf", card("new-create"), headers={"If-None-Match": "*"})[0])
        tag = self.request("GET", "/dav/contacts/concurrent-create.vcf")[1]["ETag"]
        self.assertEqual(412, self.request("PUT", "/dav/contacts/concurrent-create.vcf", card("new-create"), headers={"If-None-Match": "W/" + tag})[0])

    def test_cli_libcurl_cache_and_vdirsyncer(self):
        env = dict(self.env, XDG_CONFIG_HOME=str(self.directory / "config"), XDG_CACHE_HOME=str(self.directory / "cache"), MATES_PASSWORD="secret")
        base = f"http://127.0.0.1:{self.proxy_port}/dav/contacts/"
        subprocess.run([self.binary, "init", "--server", base, "--user", "alice"], env=env, check=True, stdout=subprocess.DEVNULL)
        # No curl executable is present in the process search path.
        env["PATH"] = str(self.directory / "no-executables")
        first = subprocess.check_output([self.binary, "list", "--remote"], env=env)
        self.assertEqual(first, subprocess.check_output([self.binary, "list", "--remote"], env=env))
        for response_file in (self.directory / "cache" / "mates" / "responses").glob("*"):
            os.utime(response_file, (time.time() - 60, time.time() - 60))
        self.proxy.terminate(); self.proxy.wait(timeout=10)
        try:
            self.assertEqual(first, subprocess.check_output([self.binary, "list", "--remote"], env=env))
        finally:
            type(self).proxy = subprocess.Popen(["nginx", "-e", str(self.directory / "nginx-start.log"), "-p", str(self.directory), "-c", str(self.directory / "nginx.conf"), "-g", "daemon off;"], stdout=subprocess.DEVNULL, stderr=self.backend_log)
            self.wait(self.proxy_port)
        local = self.directory / "vdir"
        local.mkdir(exist_ok=True)
        config = self.directory / "vdirsyncer.conf"
        config.write_text(f'''[general]
status_path = "{self.directory}/vdir-status"
[pair contacts]
a = "files"
b = "dav"
collections = null
[storage files]
type = "filesystem"
path = "{local}"
fileext = ".vcf"
[storage dav]
type = "carddav"
url = "{base}"
auth = "basic"
username = "alice"
password = "secret"
''')
        for command in ("discover", "sync"):
            result = subprocess.run(["vdirsyncer", "-c", str(config), command, "contacts"], input="y\n", text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            self.assertEqual(0, result.returncode, result.stdout)
        self.assertTrue(list(local.glob("*.vcf")))
        (local / "vdir-created.vcf").write_bytes(card("vdir-created", "From vdirsyncer"))
        result = subprocess.run(["vdirsyncer", "-c", str(config), "sync", "contacts"], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        self.assertEqual(0, result.returncode, result.stdout)
        self.assertIn(b"From vdirsyncer", self.query("/dav/contacts/")[2])
        calendar_dir = self.directory / "calendar-vdir"
        calendar_dir.mkdir()
        calendar_config = self.directory / "calendar-vdirsyncer.conf"
        calendar_config.write_text(config.read_text().replace("[pair contacts]", "[pair calendar]").replace('type = "carddav"', 'type = "caldav"').replace(base, f"http://127.0.0.1:{self.proxy_port}/dav/calendars/personal/").replace(str(local), str(calendar_dir)).replace('fileext = ".vcf"', 'fileext = ".ics"'))
        for command in ("discover", "sync"):
            result = subprocess.run(["vdirsyncer", "-c", str(calendar_config), command, "calendar"], input="y\n", text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            self.assertEqual(0, result.returncode, result.stdout)
        self.assertTrue(list(calendar_dir.glob("*.ics")))
        (calendar_dir / "vdir-event.ics").write_bytes(event("vdir-event", "DTSTART:20261028T120000Z\r\nSUMMARY:From calendar vdirsyncer\r\n"))
        result = subprocess.run(["vdirsyncer", "-c", str(calendar_config), "sync", "calendar"], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        self.assertEqual(0, result.returncode, result.stdout)
        self.assertIn(b"From calendar vdirsyncer", self.query("/dav/calendars/personal/", calendar=True)[2])


if __name__ == "__main__":
    unittest.main(verbosity=2)
