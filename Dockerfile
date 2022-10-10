FROM debian:buster

ADD Supervisor /usr/bin/Supervisor

VOLUME /data
WORKDIR /data

ENTRYPOINT ["/usr/bin/Supervisor"]
