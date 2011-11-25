# Varibles may needed to change
INC_PATH   = -I/opt/local/include/mmseg
LIB_PATH   = -L/opt/local/lib
VERSION    = $(BUILD_TYPE)-0.0.1

CC  = clang
CXX = clang++
LD  = clang++
MKDIR = mkdir

TARGET = ../priv/mmseg.exec
SRC = \
	main.cpp

OBJS = \
	main.opp 

HEADERS = 
BUILD_TYPE = debug
CFLAGS_release = -Wall -O3 -g3 -c -DNDEBUG
CFLAGS_debug   = -Wall -O0 -g3 -c -DDEBUG
 
override CFLAGS  +=  $(CFLAGS_$(BUILD_TYPE)) $(INC_PATH) -D_VERSION_STRING=$(VERSION)

LDFLAGS_release = 
LDFLAGS_debug   = 
override LDFLAGS = $(LDFLAGS_$(BUILD_TYPE)) $(LIB_PATH) -lmmseg

all: $(TARGET)

clean:
	-rm -rf $(TARGET) $(OBJS)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -o "$@" "$<"

%.opp: %.cpp $(HEADERS) 
	$(CXX) $(CFLAGS) -o "$@" "$<"

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) -o "$@" $(OBJS) 

.PHONY: all clean 

	
