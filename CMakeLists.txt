cmake_minimum_required(VERSION 3.10)
project(untitled74 C)

set(CMAKE_C_STANDARD 11)

add_executable(untitled74 main.c codegen.c gemcc.h parse.c tokenize.c type.c util.c initializer.c node.c token.c)