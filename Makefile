test:
			./test.sh

docker-build:
			docker build -t nanoscope:1 .

docker-run:
			docker run -it -v $(CURDIR):/nanoscope --name nanoscope nanoscope:1
			
docker-rm:
			docker rm nanoscope

.PHONY: test
