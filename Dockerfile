FROM clojure:lein-2.5.0
MAINTAINER Jim Ahn ahnj@yahoo.com

# grab rlwrap
RUN apt-get update && apt-get install -y rlwrap

# until repo goes public, we have to use this.
# http://stackoverflow.com/questions/23391839/clone-private-git-repo-with-dockerfile
RUN git clone https://3376880cd7bfdab8103e9f5fa94e404ac818c0cb@github.com/natemc/qiss
#
# use below when repo goes public
# RUN git clone https://@github.com/natemc/qiss

WORKDIR qiss

# pull down the dependencies
RUN lein deps 
# dump the built dependencies to the image
ADD target .

#CMD rlwrap lein run
CMD lein run
