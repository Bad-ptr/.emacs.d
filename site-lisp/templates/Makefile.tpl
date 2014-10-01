## Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
## 
## Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
## Date: (>>>VC_DATE<<<)
## License: GPL either version 2 or any later version


SOURCE_TOP_DIR  := . # source
SOURCE_SUB_DIRS :=   #shared math gfx

BUILD_TOP_DIR := bin/release

SOURCE_EXT := c #cpp
OBJECT_EXT := o

COMPILER   := $(CC) #$(CXX)
LINKER	   := $(COMPILER)

COMPILER_FLAGS := -Wall -ggdb -pipe -std=c11 $(CFLAGS) #$(CXXFLAGS) -std=c++14 -pthread `sdl-config --cflags`
LINKER_FLAGS   := -s -pipe -L`gcc -print-file-name=` #-std=c++14 -pthread
LIBS           := #-lgcc -lc -lm -lpthread #-lGL -lGLU -lGLEW `sdl-config --libs`

SOURCE_DIRS  := $(addprefix $(SOURCE_TOP_DIR)/, $(SOURCE_SUBDIRS)) $(SOURCE_TOP_DIR)
BUILD_DIRS   := $(patsubst $(SOURCE_TOP_DIR)%, $(BUILD_TOP_DIR)%, $(SOURCE_DIRS))
SOURCE_FILES := $(wildcard $(addsuffix /*.$(SOURCE_EXT), $(SOURCE_DIRS)))
OBJECT_FILES := $(patsubst $(SOURCE_TOP_DIR)%.$(SOURCE_EXT), $(BUILD_TOP_DIR)%.$(OBJECT_EXT), $(SOURCE_FILES))


# example of define, info and eval
# define SOURCE_AND_OBJECT_FILES
# 	SOURCE_FILES := $$(wildcard $$(addsuffix /*.$(SOURCE_EXT), $$(SOURCE_DIRS)))
# 	OBJECT_FILES := $$(patsubst $$(SOURCE_TOP_DIR)%.$(SOURCE_EXT), $$(BUILD_TOP_DIR)%.$(OBJECT_EXT), $$(SOURCE_FILES))
# endef
# $(info $(call SOURCE_AND_OBJECT_FILES))
# $(eval $(call SOURCE_AND_OBJECT_FILES))


(>>>POINT<<<)


$(info [START: $(shell date -R)])

all: main
	@echo "[FINISH: `date -R`]"

main: $(OBJECT_FILES)
	$(info [$(LINKER)] $^ => $@)
	$(LINKER) $^ -o $(BUILD_TOP_DIR)/$@ $(LINKER_FLAGS) $(LIBS)

$(OBJECT_FILES): | $(BUILD_DIRS)
$(BUILD_DIRS):
	@echo "[mkdir] $@"
	@mkdir -p $@

$(BUILD_TOP_DIR)/%.$(OBJECT_EXT): $(SOURCE_TOP_DIR)/%.$(SOURCE_EXT)
	@echo "[$(COMPILER)] $< -> $@"
	$(COMPILER) -c $< -o $@ $(COMPILER_FLAGS)



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


### (>>>FILE<<<) ends here
