#!/bin/sh

export DWIM_PROJECT_NAME="bt-mqtt-gateway"
#export DWIM_HTTP_SERVER_PORT="8080"
export DWIM_SWANK_PORT="13000"
export DWIM_MAXIMUM_MEMORY_SIZE="500" # in megabytes; goes straight to sbcl
#export DWIM_DAEMON_USER="home-service"

export DWIM_INSTALL_PATH="/opt/${DWIM_PROJECT_NAME}"
#export DWIM_WORKSPACE="${DWIM_INSTALL_PATH}/workspace"
#export DWIM_LOG_DIRECTORY="/var/log/${DWIM_PROJECT_NAME}"
export DWIM_LOG_DIRECTORY="/tmp"
export DWIM_EXECUTABLE_CORE_FILE="${DWIM_INSTALL_PATH}/${DWIM_PROJECT_NAME}"
#export DWIM_BACKUP_DIRECTORY="/opt/${DWIM_PROJECT_NAME}/backup/"
