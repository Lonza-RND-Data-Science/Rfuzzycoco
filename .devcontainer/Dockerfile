FROM ubuntu:24.04
ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt -y install \
    cmake \
    curl \
    git \
    build-essential

RUN apt update && apt upgrade -y && apt install -y --no-install-recommends curl locales ca-certificates 
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen && locale-gen en_US.utf8 && /usr/sbin/update-locale 
ENV LANG=en_US.UTF-8

#################### R ########################
COPY setup_base_r.sh /tmp/
RUN bash /tmp/setup_base_r.sh
RUN R --version 
COPY setup_r2u.sh /tmp
RUN bash /tmp/setup_r2u.sh

COPY setup_bspm.sh /tmp
RUN bash /tmp/setup_bspm.sh

RUN apt update && apt install -y r-cran-rcpp  r-cran-rcppprogress r-cran-srcpkgs r-cran-languageserver r-cran-httpgd

COPY install_r_pkgs.R  /usr/local/bin/install_r_pkgs
COPY r_deps.txt /tmp/r_deps.txt
RUN install_r_pkgs < /tmp/r_deps.txt

RUN apt update && apt install -y r-cran-rcpp  r-cran-rcppprogress r-cran-srcpkgs r-cran-languageserver r-cran-httpgd

# pkgdown deps;
RUN apt update && apt install -y pandoc qpdf

############# user stuff ###########
RUN apt update && apt install -y bash-completion make

ENV USER=ubuntu
# RUN useradd -s /bin/bash -m $USER
# RUN usermod -a -G staff $USER

RUN apt install -y sudo
RUN usermod -a -G sudo $USER
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER $USER
WORKDIR /home/$USER
