FROM debian:jessie
MAINTAINER Lv Zheng

RUN apt-get update && apt-get install -y \
	fortunes

COPY lazyfortune.sh /opt/
COPY lazyfortune-log.sh /opt/

RUN chmod +x /opt/lazyfortune.sh /opt/lazyfortune-log.sh

CMD ["/opt/lazyfortune-log.sh"]
