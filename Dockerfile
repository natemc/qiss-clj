# docker cheat-sheet
#
#  1. build 
#        make  # this will build and tag the image
#
#  2. *create* and launch a new container 
#        rlwrap docker-compose run qissdev lein run  # (control-c or exit to stop the container)
#           
#        note that the reason why we don't use the more straightfowrad 'docker-compose up' is 
#        due to funky interaction between docker and qiss terminal which triggers the container
#        to exit immediately upon first prompt (TODO - fix)
#
#  3. resume the above container (without creating a _new_ containter)
#        docker start  qiss_qissdev_run_1    #  skip this step if already running
#        docker attach qiss_qissdev_run_1    # 'docker ps' to find container name
#
#  3. run an _additional_ command on an running container 
#	docker exec -it qiss_qissdev_run_1 bash   # provides a bash prompt into the container in flight
#
#  **** be sure not to repeat #2 more than once, otherwise, docker will happily pump out brand-spanking new
#       containers.  unless you explicitly need a virgin container, resume using 3.
#       'docker ps -a' displays both running and stopped containers.


FROM clojure:lein-2.5.0
MAINTAINER Jim Ahn ahnj@yahoo.com

# grab rlwrap and vim
RUN apt-get update && apt-get install -y rlwrap vim

# until repo goes public, we have to use this.
# http://stackoverflow.com/questions/23391839/clone-private-git-repo-with-dockerfile
RUN git clone https://3376880cd7bfdab8103e9f5fa94e404ac818c0cb@github.com/natemc/qiss # hi
#
# use below when repo goes public
# RUN git clone https://@github.com/natemc/qiss

WORKDIR qiss

# pull down the dependencies
RUN lein deps 
# dump the built dependencies to the image
# ADD target .

# clojurescript begins here
# RUN mkdir -p hello_world/src/hello_world && wget https://github.com/clojure/clojurescript/releases/download/r3308/cljs.jar && mkdir -p src/hello_world
#
# clojurescript ends here

#CMD rlwrap lein run - lein does not like rlwrap, wrap around the docker process
CMD lein run
