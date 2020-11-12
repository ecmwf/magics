# HOWTO:
# To start the VM, "vagrant up"
# Then add the output of "vagrant ssh-config"
# to your ~/.ssh/config

$script = <<-SCRIPT
sudo dnf config-manager --set-enabled PowerTools
sudo yum update -y
sudo yum install -y git
sudo yum install -y ninja-build
sudo yum install -y netcdf-devel
sudo yum install -y cairo-devel
sudo yum install -y proj-devel
sudo yum install -y pango-devel
sudo yum install -y udunits2-devel

cmake=$(cmake --version|head -1|awk '{print $3;}')
echo "CMAKE version [$cmake]"
cmake --version
which cmake
if [[ "$cmake" != "3.15.2" ]]
then
  wget https://github.com/Kitware/CMake/releases/download/v3.15.2/cmake-3.15.2.tar.gz
  tar -zxvf cmake-3.15.2.tar.gz
  cd cmake-3.15.2
  ./bootstrap
  make
  sudo make install
fi
cd
rm -fr git build
mkdir -p git build
cd git
[[ -d ecbuild ]] || git clone git@github.com:ecmwf/ecbuild.git
git clone git@github.com:ecmwf/eccodes.git
git clone git@github.com:b8raoult/magics.git
cd ~/git/eccodes
git pull
cd ~/build
mkdir -p eccodes
cd eccodes
set -
~/git/ecbuild/bin/ecbuild -G Ninja -DENABLE_FORTRAN=0 -DENABLE_MEMFS=1 ~/git/eccodes
cmake --build .
sudo cmake --build . --target install
cd ~/build
mkdir -p magics
cd magics
~/git/ecbuild/bin/ecbuild -G Ninja -DENABLE_FORTRAN=0 -DENABLE_PYTHON=0 ~/git/magics
cmake --build .
SCRIPT


Vagrant.configure("2") do |config|

  config.vm.box = "generic/centos8"
  config.vm.synced_folder ".", "/vagrant", disabled: true

  config.vm.provision "file", source: "~/.ssh/id_rsa", destination: "~/.ssh/id_rsa"
  config.vm.provision "file", source: "~/.ssh/id_rsa.pub", destination: "~/.ssh/id_rsa.pub"
  config.vm.provision "file", source: "~/.ssh/known_hosts", destination: "~/.ssh/known_hosts"

  config.vm.provision "shell", inline: $script, privileged: false

  config.vm.provider "virtualbox" do |v|
    v.memory = 8192
    v.cpus = 4
  end

end
