VERSION 0.8
ARG --global GHC_VER=9.10.0.20240412
FROM --platform=linux/amd64 ghcr.io/konn/ghc-wasm-earthly:${GHC_VER}
WORKDIR /workdir

ENV GHC=wasm32-wasi-ghc
ENV CABAL=wasm32-wasi-cabal --project-file=cabal-wasm.project --with-ghc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs --with-ghc=wasm32-wasi-ghc-${GHC_VER}

BUILD:
  FUNCTION
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  ENV MOUNT_GLOBAL_STORE="type=cache,mode=0777,id=${target}#ghc-${GHC_VER}#global-store,sharing=shared,target=/root/.ghc-wasm/.cabal/store"
  ENV MOUNT_DIST_NEWSTYLE="type=cache,mode=0777,id=${target}#ghc${GHC_VER}#dist-newstyle,sharing=shared,target=dist-newstyle"
  COPY --keep-ts . .
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} update hackage.haskell.org,2024-04-28T21:10:00Z
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
  RUN --mount ${MOUNT_DIST_NEWSTYLE} ${WASM_LIB}/post-link.mjs --input ${HS_WASM_PATH} --output ghc_wasm_jsffi.js
  RUN --mount ${MOUNT_DIST_NEWSTYLE} wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/${wasm} "${HS_WASM_PATH}"
  RUN wasm-opt -Oz dist/${wasm} -o dist/${wasm}
  RUN wasm-tools strip -o dist/${wasm} dist/${wasm}
  LET SHASUM=$(sha1sum dist/${wasm} | cut -c1-7)
  LET FINAL_WASM=${wasm%.wasm}-${SHASUM}.wasm
  RUN mv dist/${wasm} dist/${FINAL_WASM}
  RUN cp data/index.html dist/
  RUN cp data/index.js dist/index-${SHASUM}.js
  RUN sed -i "s/index.js/index-${SHASUM}.js/g" dist/index.html
  RUN sed -i "s/${wasm}/${FINAL_WASM}/g" dist/index-${SHASUM}.js
  RUN cp *.js dist/

  SAVE ARTIFACT ./dist AS LOCAL _build

frontend:
  DO +BUILD --target=soundbooth-frontend:exe:soundbooth-frontend
