# docker cheat-sheet
#
#  0. launch a "Docker CLI" from Kitematic

#  1. build 
#        docker-compose build  # this will build and tag the image
#
#  2. *create* and launch a new container 
#        docker-compose up  # (control-c or exit to stop the container)
#           
#  3. attach to the running container above, bash in
#	docker exec -it qiss_qissdev_1 bash   # provides a bash prompt into the container in flight
#
#  3. resume the above container (without creating a _new_ containter)
#        docker start  qiss_qissdev_1    #  skip this step if already running
#        docker attach qiss_qissdev_1    # 'docker ps' to find container names (also visible on the lhs of Kissmatic console) 
#
#  **** be sure not to repeat #2 more than once, otherwise, docker will happily pump out brand-spanking new
#       containers.  unless you explicitly need a virgin container, resume using 3.
#       'docker ps -a' displays both running and stopped containers.
#  


FROM clojure:lein-2.5.0
MAINTAINER Jim Ahn ahnj@yahoo.com

# grab rlwrap and vim
RUN apt-get update && apt-get install -y rlwrap vim nodejs nodejs-legacy npm git git-core

# until repo goes public, we have to use this.
# http://stackoverflow.com/questions/23391839/clone-private-git-repo-with-dockerfile
# there's a implied WORKDIR of / here..
RUN git clone https://3376880cd7bfdab8103e9f5fa94e404ac818c0cb@github.com/natemc/qiss 
WORKDIR /qiss
#
# use below when repo goes public
# RUN git clone https://@github.com/natemc/qiss


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
WORKDIR /qiss/clojurescript
RUN mkdir -p src/hello_world && wget https://github.com/clojure/clojurescript/releases/download/r3308/cljs.jar && npm install source-map-support
#
#
# clojurescript ends here

EXPOSE 3000

# qiss
#WORKDIR /qiss
#CMD rlwrap lein run - lein does not like rlwrap, wrap around the docker process
#CMD lein run

# node
WORKDIR /mynodeapp 
CMD npm start
