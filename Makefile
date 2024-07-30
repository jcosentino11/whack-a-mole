
APP := whackamole

REBAR := rebar3
REL_DIR := _build

.PHONY: release
release:
	@$(REBAR) release

.PHONY: start
start: release
	@$(REL_DIR)/default/rel/$(APP)/bin/$(APP) console
