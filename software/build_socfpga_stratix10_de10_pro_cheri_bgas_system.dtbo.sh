#! /usr/bin/env sh

echo "build device tree overlay exposing CHERI BGAS system devices to the de10 pro HPS"

WORKDIR=$(pwd)/socfpga_stratix10_de10_pro_cheri_bgas_system.dtbo_build
mkdir -p $WORKDIR
cd $WORKDIR

GITHUB_BASE=https://raw.githubusercontent.com
GITHUB_USER=freebsd
GITHUB_REPO=freebsd-src
#de10_pro_revD
#COMMIT_ISH=63f1e04694ec7bafc52005c96cc466dcfe6879ea
#de10_pro_revC
COMMIT_ISH=stable/13
URL_BASE=$GITHUB_BASE/$GITHUB_USER/$GITHUB_REPO/$COMMIT_ISH
TOP="socfpga_stratix10_de10_pro_cheri_bgas_system.dtso"
SRCS="
sys/contrib/device-tree/include/dt-bindings/interrupt-controller/arm-gic.h
sys/contrib/device-tree/include/dt-bindings/interrupt-controller/irq.h
"

echo "download sources from:"
echo "$GITHUB_BASE"
echo "$GITHUB_USER"
echo "$GITHUB_REPO"
echo "$COMMIT_ISH"

for f in $SRCS; do
  curl -o ${f#sys/contrib/device-tree/include/} --create-dirs $URL_BASE/$f;
done
cp ../$TOP $WORKDIR

DTB=$(pwd)/$(basename $TOP .dtso).dtbo
cpp -I . -x assembler-with-cpp -P $TOP | dtc -H both -O dtb -o $DTB \
&& echo "device tree binary built: $DTB"
