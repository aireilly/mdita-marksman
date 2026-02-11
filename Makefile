# Allow packagers to override PREFIX with their distribution standard
PREFIX?=${HOME}/.local

# Do magic to turn make arguments into command line arguments for executable invocation
ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

# Linux-specific RID detection
UNAME_S := $(shell uname -s)
TARGET := $(shell uname -m)

ifeq ($(UNAME_S),Linux)
	OS_ID := linux
else
$(error This project only supports Linux builds)
endif

ifeq ($(TARGET),x86_64)
	ARCH_ID := x64
else ifeq ($(TARGET),aarch64)
	ARCH_ID := arm64
else
	ARCH_ID := $(TARGET)
endif

RID := $(OS_ID)-$(ARCH_ID)

.PHONY: setup
setup:
	dotnet tool restore

.PHONY: check
check: setup
	dotnet fantomas --check Marksman

.PHONY: build
build:
	dotnet build Marksman/Marksman.fsproj

.PHONY: test
test:
	dotnet test Tests/Tests.fsproj

.PHONY: clean
clean:
	dotnet clean

.PHONY: run
run:
	dotnet run --project Marksman -- $(ARGS)

.PHONY: fmt
fmt: setup
	dotnet fantomas Marksman
	dotnet fantomas Tests
	xmllint Marksman/Marksman.fsproj -o Marksman/Marksman.fsproj

.PHONY: publish
publish:
	dotnet publish -c Release -r $(RID) --self-contained true \
		-p:PublishSingleFile=true \
		-p:PublishTrimmed=true \
		-p:TrimMode=partial \
		-p:DebugType=embedded \
		-p:EnableCompressionInSingleFile=true \
		-p:UseAppHost=true \
		Marksman/Marksman.fsproj

# Install the binary to $HOME/.local/bin folder
.PHONY: install
install: publish
	mkdir -p $(PREFIX)/bin
	install -m755 Marksman/bin/Release/net10.0/$(RID)/publish/mdita-marksman $(PREFIX)/bin

.PHONY: uninstall
uninstall:
	rm -rf $(PREFIX)/bin/mdita-marksman

.DEFAULT_GOAL := build
