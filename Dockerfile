
FROM openjdk:8-jre

# that's me!
MAINTAINER Alex K, allixender@googlemail.com

LABEL app="smart-csw-ingester"
LABEL version="1.0-SNAPSHOT"
LABEL repo="https://github.com/ZGIS/smart-csw-ingester"
LABEL build_number=TRAVIS_BUILD_NUMBER

RUN apt-get update -y \
  && apt-get install -y --no-install-recommends \
	ca-certificates curl wget pwgen unzip openssl \
  && rm -rf /var/lib/apt/lists/*

ADD smart-csw-ingester-1.0-SNAPSHOT.tgz /

ENV JAVA_OPTS "-Xms192m -Xmx398m -XX:+UseParallelGC -XX:+UseParallelOldGC"

EXPOSE 9000

# -DapplyEvolutions.default=true
# -Dpidfile.path=$APP_HOME/running.pid
# -Dpidfile.path=/tmp/play.pid
# -Dapplication.base_url=http://test.smart-project.info/

# We need envs for
# ${?APPLICATION_SECRET}

CMD [ "/smart-portal-backend-1.0-SNAPSHOT/bin/smart-csw-ingester", \
    "-Dconfig.resource=application.conf", \
    "-Dhttp.address=0.0.0.0", \
    "-Dhttp.port=9000", \
    "-Dlogger.resource=logback-stdout.xml"]
