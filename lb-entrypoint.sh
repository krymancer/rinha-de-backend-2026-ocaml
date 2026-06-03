#!/bin/sh
# Wait until both API backends answer GET /ready before starting HAProxy.
# This way the published :9999 only appears once we can actually serve, so
# startup probes get connection-refused (retry-friendly) instead of HAProxy
# resetting connections while the backends are still loading the index +
# warming up. Mirrors the old fd_lb.c wait-for-backend behavior.
for be in api1 api2; do
  i=0
  while ! wget -q -O /dev/null -T 1 "http://$be:9999/ready" 2>/dev/null; do
    i=$((i + 1))
    if [ "$i" -gt 300 ]; then echo "[lb] timeout waiting for $be:9999" >&2; break; fi
    sleep 0.2
  done
  echo "[lb] $be ready" >&2
done
exec haproxy -db -f /usr/local/etc/haproxy/haproxy.cfg
