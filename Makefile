images: minimal-image 

minimal-image:
	docker build -t qiss/minimal .

upload: images
	docker push qiss/minimal 

super-nuke: nuke
	-docker rmi qiss/minimal


# Cleanup with fangs
nuke:
	-docker stop `docker ps -aq`
	-docker rm -fv `docker ps -aq`
	-docker images -q --filter "dangling=true" | xargs docker rmi

.PHONY: nuke
