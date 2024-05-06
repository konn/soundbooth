VERSION 0.8
ARG --global GHC_VER=9.10.0.20240412
FROM --platform=linux/amd64 ghcr.io/konn/ghc-wasm-earthly:${GHC_VER}
WORKDIR /workdir

ENV GHC=wasm32-wasi-ghc
ENV CABAL=wasm32-wasi-cabal --project-file=cabal-wasm.project --with-ghc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs --with-ghc=wasm32-wasi-ghc-${GHC_VER}

build:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  ENV MOUNT_GLOBAL_STORE="type=cache,mode=0777,id=${target}#ghc-${GHC_VER}#global-store,sharing=shared,target=/root/.ghc-wasm/.cabal/store"
  ENV MOUNT_DIST_NEWSTYLE="type=cache,mode=0777,id=${target}#ghc${GHC_VER}#dist-newstyle,sharing=shared,target=dist-newstyle"
  COPY --keep-ts cabal-common.project cabal-wasm.project cabal-wasm.project.freeze .
  COPY --keep-ts soundbooth-core/soundbooth-core.cabal soundbooth-core/soundbooth-core.cabal
  COPY --keep-ts soundbooth-frontend/soundbooth-frontend.cabal soundbooth-frontend/soundbooth-frontend.cabal
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} update hackage.haskell.org,2024-04-28T21:10:00Z
  COPY --keep-ts soundbooth-core soundbooth-core
  COPY --keep-ts soundbooth-frontend soundbooth-frontend
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} build --only-dependencies ${target}
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} build  ${target}
  # From frontend/build.sh in tweag/ghc-wasm-miso-examples
  LET HS_WASM_PATH=$(${CABAL} list-bin -v0 ${target})
  LET WASM_LIB=$(wasm32-wasi-ghc --print-libdir)
  LET DEST=dist
  RUN mkdir -p dist
  RUN --mount ${MOUNT_DIST_NEWSTYLE} cp ${HS_WASM_PATH} ./dist/${wasm}
  RUN --mount ${MOUNT_DIST_NEWSTYLE} ${WASM_LIB}/post-link.mjs --input ${HS_WASM_PATH} --output ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT dist

optimised-wasm:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  RUN mkdir -p dist/
  COPY (+build/dist/${wasm}.orig --target=${target} --outdir=${outdir} --wasm=${wasm}.orig) ./dist/
  RUN wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/${wasm} dist/${wasm}.orig
  RUN wasm-opt -Oz dist/${wasm} -o dist/${wasm}
  RUN wasm-tools strip -o dist/${wasm} dist/${wasm}
  COPY (+build/dist/ghc_wasm_jsffi.js --target=${target} --outdir=${outdir} --wasm=${wasm}.orig) ./dist/
  SAVE ARTIFACT dist

frontend:
  COPY (+optimised-wasm/dist --target=soundbooth-frontend:exe:soundbooth-frontend) ./dist
  LET ORIG_WASM=soundbooth-frontend.wasm
  LET SHASUM_WASM=$(sha1sum dist/${ORIG_WASM} | cut -c1-7)
  LET FINAL_WASM=soundbooth-frontend-${SHASUM_WASM}.wasm
  RUN mv dist/${ORIG_WASM} dist/${FINAL_WASM}

  LET GHC_JSFFI_ORIG=ghc_wasm_jsffi.js
  LET SHASUM_JSFFI=$(sha1sum dist/${GHC_JSFFI_ORIG} | cut -c1-7)
  LET GHC_JSFFI_FINAL=ghc_wasm_jsffi-${SHASUM_JSFFI}.js
  RUN mv dist/${GHC_JSFFI_ORIG} dist/${GHC_JSFFI_FINAL}

  COPY data/index.js dist/index.js
  RUN sed -i "s/${ORIG_WASM}/${FINAL_WASM}/g" dist/index.js
  RUN sed -i "s/${GHC_JSFFI_ORIG}/${GHC_JSFFI_FINAL}/g" dist/index.js
  LET INDEX_JS_SHASUM=$(sha1sum dist/index.js | cut -c1-7)
  LET INDEX_JS_FINAL=index-${INDEX_JS_SHASUM}.js
  RUN mv dist/index.js dist/${INDEX_JS_FINAL}
  COPY data/index.html dist/index.html
  COPY data/imgs dist/imgs
  RUN sed -i "s/index.js/${INDEX_JS_FINAL}/g" dist/index.html
  RUN rm dist/*.orig

  SAVE ARTIFACT ./dist AS LOCAL _build
