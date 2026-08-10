#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
set -euo pipefail
killall -q polybar 2>/dev/null || true
sleep 0.2
polybar main 2>&1 | tee /tmp/polybar.log &
