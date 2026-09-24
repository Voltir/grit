# grit's Postgres: the official postgres:18 image plus Tiger Data's pg_textsearch (BM25).
# docker-compose.yml builds it for the dev database, and TestPostgres builds the same file
# for the live tests (Testcontainers), so both run one image.
#
# The extension comes from the upstream release's prebuilt .deb, verified against the
# sha256 digests GitHub publishes for each release asset (the release has no separate
# checksum file). Architecture is read from dpkg rather than BuildKit's TARGETARCH, because
# Testcontainers builds through the classic builder, which does not set it.
#
# Nothing here reaches the network on a rebuild: the download is a RUN whose text names
# the version and digests, so Docker's layer cache holds it until one of them changes.

# The major is written out in FROM rather than taken from an ARG: Testcontainers pre-fetches
# the base images it reads in FROM lines and cannot substitute an ARG there.
FROM postgres:18 AS fetch
ARG PG_MAJOR=18
ARG PG_TEXTSEARCH_VERSION=1.4.0
ARG PG_TEXTSEARCH_SHA256_AMD64=c2ce0585a7f09ec7fc441f1fba11888d6ad3ab51b1851d4f24a7457905d0519f
ARG PG_TEXTSEARCH_SHA256_ARM64=f65bc4f46393ca66c78e4da71ae8c54d0aef9975eaf3d5b6836d0f4570c68c64
RUN set -eu; \
    arch="$(dpkg --print-architecture)"; \
    case "$arch" in \
      amd64) sha="$PG_TEXTSEARCH_SHA256_AMD64" ;; \
      arm64) sha="$PG_TEXTSEARCH_SHA256_ARM64" ;; \
      *) echo "pg_textsearch publishes no build for $arch" >&2; exit 1 ;; \
    esac; \
    apt-get update; \
    apt-get install -y --no-install-recommends ca-certificates curl unzip; \
    curl -fsSL -o /tmp/pgts.zip \
      "https://github.com/timescale/pg_textsearch/releases/download/v${PG_TEXTSEARCH_VERSION}/pg-textsearch-v${PG_TEXTSEARCH_VERSION}-pg${PG_MAJOR}-${arch}.zip"; \
    echo "$sha  /tmp/pgts.zip" | sha256sum -c -; \
    unzip -q /tmp/pgts.zip -d /tmp/pgts; \
    dpkg-deb -x "/tmp/pgts/pg-textsearch-postgresql-${PG_MAJOR}_${PG_TEXTSEARCH_VERSION}-1_${arch}.deb" /out; \
    test -f "/out/usr/lib/postgresql/${PG_MAJOR}/lib/pg_textsearch.so"

FROM postgres:18
ARG PG_MAJOR=18
COPY --from=fetch /out/usr/ /usr/
# Preloaded two ways, because each alone misses a case:
# - the sample initdb copies covers a new cluster whose command line is replaced (the tests
#   pass their own `postgres -c ...`);
# - the command line covers a cluster initialised before this image existed (a
#   docker-compose volume created from plain postgres:18), whose postgresql.conf the sample
#   never reaches.
RUN echo "shared_preload_libraries = 'pg_textsearch'" >> "/usr/share/postgresql/${PG_MAJOR}/postgresql.conf.sample"
CMD ["postgres", "-c", "shared_preload_libraries=pg_textsearch"]
