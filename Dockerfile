FROM ubuntu:20.04

RUN apt update
RUN apt install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN echo "export PATH=/root/.local/bin:/usr/local/bin:$PATH" >> /root/.bashrc
RUN stack setup
RUN DEBIAN_FRONTEND=noninteractive apt install -y software-properties-common
RUN apt-add-repository "deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-9 main" || true
RUN apt install -y clang-9 clang-tools-9 clang-tidy-9 clang-format-9 lldb-9 lld-9 
RUN ln -s /usr/bin/clang-9 /usr/bin/clang
RUN ln -s /usr/bin/lli-9 /usr/bin/lli

ADD . /nanoscope
WORKDIR /nanoscope