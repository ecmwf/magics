# Slim Docker multi-stage build
# for Magics

# Build image
FROM python:3.7.10-slim-buster as build

RUN set -ex \
    && apt-get update

# Install tools
RUN set -ex \
    && apt-get install --yes --no-install-suggests --no-install-recommends \
        wget \
        git

# Install build tools.
RUN set -ex \
    && apt-get install --yes --no-install-suggests --no-install-recommends \
      bison \
      bzip2 \
      ca-certificates \
      cmake \
      curl \
      file \
      flex \
      g++-8 \
      gcc-8 \
      gfortran-8 \
      git \
      make \
      patch \
      sudo \
      swig \
      xz-utils

RUN set -ex \
    && ln -s /usr/bin/g++-8 /usr/bin/g++ \
    && ln -s /usr/bin/gcc-8 /usr/bin/gcc \
    && ln -s /usr/bin/gfortran-8 /usr/bin/gfortran

# Install build-time dependencies.
RUN set -ex \
    && apt-get install --yes --no-install-suggests --no-install-recommends \
      libarmadillo-dev \
      libatlas-base-dev \
      libbz2-dev \
      libc6-dev \
      libcairo2-dev \
      libcurl4-openssl-dev \
      libeigen3-dev \
      libexpat1-dev \
      libfreetype6-dev \
      libfribidi-dev \
      libgdal-dev \
      libgeos-dev \
      libharfbuzz-dev \
      libhdf5-dev \
      libjpeg-dev \
      liblapack-dev \
      libncurses5-dev \
      libnetcdf-dev \
      libpango1.0-dev \
      libpcre3-dev \
      libpng-dev \
      libreadline6-dev \
      libsqlite3-dev \
      libssl-dev \
      libxml-parser-perl \
      libxml2-dev \
      libxslt1-dev \
      libyaml-dev \
      sqlite3 \
      zlib1g-dev

# Install Proj6

RUN mkdir -p /proj6/src \
    && cd /proj6/src \
    && wget https://download.osgeo.org/proj/proj-6.3.1.tar.gz \
    && tar -xf proj-6.3.1.tar.gz \
    && mkdir -p /proj6/build/ \
    && cd /proj6/build \
    && cmake /proj6/src/proj-6.3.1 \
        -DCMAKE_BUILD_TYPE=Release \
        -DPROJ_TESTS=OFF \
    && make -j4 \
    && make install



# Install Python run-time dependencies.
COPY requirements.txt /root/

RUN set -ex \
    && pip install -r /root/requirements.txt

# Install ecbuild
ARG ECBUILD_VERSION=2021.03.0
RUN set -eux \
    && mkdir -p /src/ \
    && cd /src \
    && git clone https://github.com/ecmwf/ecbuild.git \
    && cd ecbuild \
    && git checkout ${ECBUILD_VERSION} \
    && mkdir -p /build/ecbuild \
    && cd /build/ecbuild \
    && cmake /src/ecbuild -DCMAKE_BUILD_TYPE=Release \
    && make -j4 \
    && make install

# Install eccodes
# requires ecbuild
ARG ECCODES_VERSION=2021.03.0
RUN set -eux \
    && mkdir -p /src/ \
    && cd /src \
    && git clone https://github.com/ecmwf/eccodes.git \
    && cd eccodes \
    && git checkout ${ECCODES_VERSION} \
    && mkdir -p /build/eccodes \
    && cd /build/eccodes \
    && /usr/local/bin/ecbuild /src/eccodes -DECMWF_GIT=https -DCMAKE_BUILD_TYPE=Release \
    && make -j4 \
    && make install \
    && /sbin/ldconfig

# Install Magics
# requires ecbuild, eccodes

COPY . /src/magics

RUN set -eux \
    && mkdir -p /build/magics/ \
    && cd /build/magics \
    && /usr/local/bin/ecbuild /src/magics -DECMWF_GIT=https -DCMAKE_BUILD_TYPE=Release \
    && make -j4 \
    && make install \
    && /sbin/ldconfig

# Remove unneeded files.
RUN set -ex \
    && find /usr/local -name 'lib*.so' | xargs -r -- strip --strip-unneeded || true \
    && find /usr/local/bin | xargs -r -- strip --strip-all || true \
    && find /usr/local/lib -name __pycache__ | xargs -r -- rm -rf

#
# Run-time image.
#
FROM python:3.7.10-slim-buster

# Install run-time depencencies.
# Delete resources after installation
RUN set -ex \
    && apt-get update \
    && apt-get install --yes --no-install-suggests --no-install-recommends \
       ca-certificates \
       curl \
       ghostscript \
       imagemagick \
       ksh \
       libarmadillo9 \
       libbz2-1.0 \
       libcairo-gobject2 \
       libcairo-script-interpreter2 \
       libcairo2 \
       libcroco3 \
       libcurl4 \
       libexif12 \
       libexpat1 \
       libfontconfig1 \
       libfreetype6 \
       libfribidi0 \
       libgdal20 \
       libgeoip1 \
       libgeos-c1v5 \
       libgif7 \
       libgomp1 \
       libgssrpc4 \
       libharfbuzz0b \
       libhdf5-103 \
       libicu63    \
       libilmbase23 \
       libjbig0 \
       libjpeg62-turbo \
       libjs-jquery \
       liblcms2-2 \
       liblqr-1-0 \
       libncurses5 \
       libnetcdf13 \
       libopenexr23 \
       libpangocairo-1.0-0 \
       libpangoxft-1.0-0 \
       libpcre3 \
       libpcrecpp0v5 \
       libpng16-16 \
       libreadline7 \
       libsqlite3-0 \
       libssl1.1 \
       libtiff5 \
       libtiffxx5 \
       libwebp6 \
       libxft2 \
       libxml2 \
       libxslt1.1 \
       poppler-utils \
       rsync \
       zlib1g \
    && rm -rf /var/lib/apt/lists/*

# Copy Python run-time and ECMWF softwate.
COPY --from=build /usr/local/share/eccodes/ /usr/local/share/eccodes/
COPY --from=build /usr/local/share/magics/ /usr/local/share/magics/
COPY --from=build /usr/local/share/proj/ /usr/local/share/proj/
COPY --from=build /usr/local/bin/ /usr/local/bin/
COPY --from=build /usr/local/lib/ /usr/local/lib/

# Ensure shared libs installed by the previous step are available.
RUN set -ex \
    && /sbin/ldconfig

# Configure Python runtime.
ENV \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

# Run selfcheck
CMD python -m Magics selfcheck

# METADATA
# Build-time metadata as defined at http://label-schema.org
# --build-arg BUILD_DATE=`date -u +"%Y-%m-%dT%H:%M:%SZ"`
ARG BUILD_DATE
# --build-arg VCS_REF=`git rev-parse --short HEAD`, e.g. 'c30d602'
ARG VCS_REF
# --build-arg VCS_URL=`git config --get remote.origin.url`, e.g. 'https://github.com/eduardrosert/docker-magics'
ARG VCS_URL
# --build-arg VERSION=`git tag`, e.g. '0.2.1'
ARG VERSION
LABEL org.label-schema.build-date=$BUILD_DATE \
        org.label-schema.name="Magics" \
        org.label-schema.description="Magics is the latest generation of the ECMWF's meteorological plotting software and can be either accessed directly through its Python or Fortran interfaces or by using Metview." \
        org.label-schema.url="https://confluence.ecmwf.int/display/MAGP/Magics" \
        org.label-schema.vcs-ref=$VCS_REF \
        org.label-schema.vcs-url=$VCS_URL \
        org.label-schema.vendor="ECMWF - European Centre for Medium-Range Weather Forecasts" \
        org.label-schema.version=$VERSION \
        org.label-schema.schema-version="1.0"
