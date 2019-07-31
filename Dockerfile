FROM ubuntu:bionic

WORKDIR /code

# Setup base image deps
RUN apt-get update && apt-get install -y \
    build-essential \
    gcc-multilib \
    libncurses5-dev \
    libx11-dev \
    uuid-dev \
    wget \
    zsh \
    neovim \
    && rm -rf /var/lib/apt/lists/*

# Install chez from source
RUN cd /tmp \
    && wget -q https://github.com/cisco/ChezScheme/releases/download/v9.5.2/csv9.5.2.tar.gz \
    && tar -xf csv9.5.2.tar.gz \
    && cd csv9.5.2 \
    && ./configure \
    && make install \
    && cd - \
    && rm -rf csv9.5.2.tar.gz csv9.5.2

ADD . /code

ENTRYPOINT ["/bin/bash"]
