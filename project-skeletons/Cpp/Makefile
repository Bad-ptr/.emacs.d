## Copyright (C) __YEAR__ __USER-NAME__
##
## Author: __USER-NAME__ (__USER-NICKNAME__) <(__USER-MAIL-ADDRESS__)>
## Date: __DATE-TIME__
## License: __LICENSE__


MODE :=debug

SOURCE_TOP_DIR  :=.
#src
SOURCE_SUB_DIRS :=
#shared math gfx

BUILD_TOP_DIR :=bin/$(MODE)

SOURCE_EXT :=cpp
OBJECT_EXT :=o

COMPILER   :=$(CXX)
COMPILER_FLAGS :=-pipe $(CXXFLAGS) -std=c++11 -pthread
#`sdl-config --cflags`

ifeq ($(MODE),debug)
COMPILER_FLAGS += -Wall -ggdb
else
COMPILER_FLAGS += -O2
endif


LINKER	   :=$(COMPILER)

LINKER_FLAGS   :=-s -L`gcc -print-file-name=`

LIBS           :=
#-lgcc -lc -lm -lpthread #-lGL -lGLU -lGLEW `sdl-config --libs`


SOURCE_DIRS  :=$(addprefix $(SOURCE_TOP_DIR)/, $(SOURCE_SUB_DIRS)) $(SOURCE_TOP_DIR)
BUILD_DIRS   :=$(patsubst $(SOURCE_TOP_DIR)%, $(BUILD_TOP_DIR)%, $(SOURCE_DIRS))
SOURCE_FILES :=$(wildcard $(addsuffix /*.$(SOURCE_EXT), $(SOURCE_DIRS)))
OBJECT_FILES :=$(patsubst $(SOURCE_TOP_DIR)%.$(SOURCE_EXT), $(BUILD_TOP_DIR)%.$(OBJECT_EXT), $(SOURCE_FILES))






$(info [STARTING: $(shell date -R)])

all: main
	@echo "[FINISHED BUILDING: `date -R`]"

main: $(OBJECT_FILES)
	@echo "[FINISHED COMPILING: `date -R`]"
	$(info [$(LINKER)] $^ => $@)
	@$(LINKER) $(COMPILER_FLAGS) $(LINKER_FLAGS) $^ $(LIBS) -o $(BUILD_TOP_DIR)/$@ && echo "[FINISHED LINKING: `date -R`]"

$(OBJECT_FILES): | $(BUILD_DIRS)
$(BUILD_DIRS):
	@echo "[mkdir] $@"
	@mkdir -p $@

$(BUILD_TOP_DIR)/%.$(OBJECT_EXT): $(SOURCE_TOP_DIR)/%.$(SOURCE_EXT)
	@echo "[$(COMPILER)] $< -> $@"
	@$(COMPILER) $(COMPILER_FLAGS) -c $< -o $@



.PHONY: mkdirs clean showvars rebuild


mkdirs:
	@echo [mkdir] -p $(BUILD_DIRS)
	@mkdir -p $(BUILD_DIRS)

clean:
	@echo cleaning...
	@echo [rm] $(OBJECT_FILES)
	@rm $(OBJECT_FILES)
	@echo [rm] $(BUILD_TOP_DIR)/main
	@rm $(BUILD_TOP_DIR)/main

showvars:
	@echo SOURCE_DIRS : $(SOURCE_DIRS)
	@echo SOURCE_FILES: $(SOURCE_FILES)
	@echo OBJECT_FILES: $(OBJECT_FILES)

rebuild: clean all


### Makefile ends here
