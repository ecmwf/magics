

# test
TEST_CMD := python -m Magics selfcheck

# load variables from ./hooks/env
DATE := ${shell . ./hooks/env && echo $$DATE}
SOURCE_URL := ${shell . ./hooks/env && echo $$SOURCE_URL}
SOURCE_BRANCH := ${shell . ./hooks/env && echo $$SOURCE_BRANCH}
SOURCE_COMMIT := ${shell . ./hooks/env && echo $$SOURCE_COMMIT}
SOURCE_TAG := ${shell . ./hooks/env && echo $$SOURCE_TAG}
DOCKER_TAG := ${shell . ./hooks/env && echo $$DOCKER_TAG}
IMAGE_NAME := ${shell . ./hooks/env && echo $$IMAGE_NAME}

all: build

.PHONY: build run run-interactive test login login-user-pass push clean

build:
	./hooks/build

run:
	@docker run --rm -i -t ${IMAGE_NAME}

run-interactive:
	@docker run --rm -i -t ${IMAGE_NAME} sh

test:
	@docker run --rm -i -t ${IMAGE_NAME} ${TEST_CMD}

login:
	@docker login

login-user-pass:
	@docker login -u ${DOCKER_USER} -p ${DOCKER_PASS}

push: login
	@docker push ${DOCKER_REPO}

clean:
	@docker rmi ${DOCKER_REPO}