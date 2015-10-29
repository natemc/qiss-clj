FROM clojure:lein-2.5.0
MAINTAINER Jim Ahn ahnj@yahoo.com

# grab rlwrap and vim
RUN apt-get update && apt-get install -y rlwrap vim nodejs nodejs-legacy npm git git-core

# there's a implied WORKDIR of / here..
RUN git clone https://github.com/natemc/qiss.git
WORKDIR /qiss

# pull down the dependencies
RUN lein deps 
# dump the built dependencies to the image
# ADD target .

# clojurescript begins here
#
# nodejs - https://www.airpair.com/node.js/posts/getting-started-with-docker-for-the-nodejs-dev 
# 
WORKDIR /
RUN npm install -g express-generator
RUN express mynodeapp  && cd mynodeapp && npm install # don't try to combine this line with the above!  docker bug?
#
# clojurescript - quickstart - https://github.com/clojure/clojurescript/wiki/Quick-Start 
#
WORKDIR /qiss
#RUN mkdir -p src/hello_world && wget https://github.com/clojure/clojurescript/releases/download/r3308/cljs.jar && npm install source-map-support
#
#
# clojurescript ends here

EXPOSE 3449

# lein does not like rlwrap, wrap around the docker process instead
#  (for example - rlwrap docker-compose up qissrepl)
CMD lein run

# node
#WORKDIR /mynodeapp 
#CMD start
