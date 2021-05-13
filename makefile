init:
    -git clone --recurse-submodule https://github.com/shadowrylander/shadowrylander home/shadowrylander
.DEFAULT_GOAL := init

rebuild:
    chmod +x ./wheee
    ./wheee --use-hash ${HASH} -H make

switch:
    chmod +x ./wheee
    ./wheee --use-hash ${HMASH} -H make --home-manager
    ./wheee --use-hash ${RMASH} -H make --home-manager
