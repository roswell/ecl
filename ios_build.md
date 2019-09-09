## Steps to build for iOS arm64

1. Checkout branch arm64-port

2. Customize ./mac_build.sh and run

  Change configure options as you like, i.e. installation prefix.

3. cd build_mac/ && make install

  Need it later because dpp/cmp are changed.

4. Modify cross_config_ios_arm64

  Change `ECL_TO_RUN` as previously installed path.

5. run ./ios_build_arm64.sh

  Built in build_ios_arm64, and installed in archive_ios_arm64.
