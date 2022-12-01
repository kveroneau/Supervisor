FROM kveroneau/lazarus:2.0.6-amd64 AS builder

COPY . /root

RUN cd /root && lazbuild --bm=Release Supervisor.lpr

FROM debian:buster

COPY --from=builder /root/Supervisor /usr/bin/Supervisor

ENTRYPOINT ["/usr/bin/Supervisor"]
