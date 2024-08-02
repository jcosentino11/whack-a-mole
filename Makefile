
APP := whackamole

REBAR := rebar3
REL_DIR := _build

ENV = default

.PHONY: release
release:
	@$(REBAR) as $(ENV) release

.PHONY: start
start: release
	@$(REL_DIR)/$(ENV)/rel/$(APP)/bin/$(APP) console

.PHONY: release-docker
release-docker:
	@docker build -t whackamole .

.PHONY: start-docker
start-docker: release-docker
	@docker run --rm -it -p 8080:8080 whackamole
	