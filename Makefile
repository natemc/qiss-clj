images: minimal-image 

minimal-image:
	docker build -t qiss/minimal .

upload: images
	docker push qiss/minimal 

super-nuke: nuke
	-docker rmi qiss/minimal

docker: minimal-image
	-docker run -it --rm -p 3449:3449 -v .:/qiss/home qiss/minimal:latest 

dockerfigwheel: minimal-image
	-docker run -it --rm -p 3449:3449 -v .:/qiss/home qiss/minimal:latest bash -c "lein figwheel docker"

# Cleanup with fangs
nuke:
	-docker stop `docker ps -aq`
	-docker rm -fv `docker ps -aq`
	-docker images -q --filter "dangling=true" | xargs docker rmi

.PHONY: nuke
