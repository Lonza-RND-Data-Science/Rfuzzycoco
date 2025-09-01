# install base R 4.4 using APT and  https://cloud.r-project.org/bin/linux/ubuntu/ repository
# cf  https://cloud.r-project.org/bin/linux/ubuntu/

set -euo pipefail

UBUNTU_CODENAME=noble
# R_VERSION=4.5.0-3.2204.0
curl -fsSL https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc >/dev/null
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu ${UBUNTU_CODENAME}-cran40/" >/etc/apt/sources.list.d/cran-ubuntu.list
echo "Set up https://cloud.r-project.org/bin/linux/ubuntu for R 4.4 ..."


# apt install -y --no-install-recommends  r-base=$R_VERSION r-base-dev=$R_VERSION r-base-core=$R_VERSION r-recommended=$R_VERSION
apt update && apt install -y --no-install-recommends  r-base r-base-dev r-base-core r-recommended

## Support user-level installation of R packages (if user belongs to staff group)
chown root:staff "/usr/local/lib/R/site-library"
chmod g+ws "/usr/local/lib/R/site-library"
