# bspm: Bridge to System Package Manager
# cf https://cran4linux.github.io/bspm/
# --> allow to use install.packages() to install APT r-cran-* packages
set -euo pipefail

# ## Install bspm for r2u 
apt update
apt install -y --no-install-recommends r-cran-bspm
# this also requires some python packages
apt install -y --no-install-recommends python3-{dbus,gi,apt}

## Configure bspm and set one helpful apt default

echo "options(bspm.sudo = TRUE)" >> /etc/R/Rprofile.site
# If false, as many binaries are installed as possible without any version check, and then installation from source is used as a fallback.
echo "options(bspm.version.check=FALSE)" >> /etc/R/Rprofile.site
# enable or disable the integration of install_sys into install.packages. 
echo "suppressMessages(bspm::enable())" >> /etc/R/Rprofile.site

