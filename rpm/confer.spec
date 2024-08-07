%global pkg_name confer

Name:           %{pkg_name}
Version:        0.1.0.0
Release:        1%{?dist}
Summary:        The dotfiles manager

License:        BSD-3-Clause
Url:            https://github.com/tchoutri/%{name}
Source0:        %{url}/releases/download/%{version}/confer-head-Linux-static-%{buildarch}.tar.gz

%description
Confer is a configuration file manager that symlinks your configuration files
into their appropriate locations. You can put your configuration files in
version control and make them easily available to their applications.

%prep
%setup -q

%build

%install
# Bash completion

mkdir -p %{_datadir}/bash-completion/completions/
outfile=%{_datadir}/bash-completion/completions/%{name}
if [ ! -e $outfile ]; then
  $_bindir/%{name} --bash-completion-script %{name} | sed s/filenames/default/ > $outfile
else
  echo "$outfile exists!"; exit 1
fi

# ZSH completion

mkdir -p %{_datadir}/zsh/site-functions
outfile=%{_datadir}/zsh/site-functions/_%{name}
if [ ! -e $outfile ]; then
  $_bindir/%{name} --zsh-completion-script %{name} | sed s/filenames/default/ > $outfile
else
  echo "$outfile exists!"; exit 1
fi

# Fish completion

mkdir -p %{_datadir}/fish/vendor_completions.d
outfile=%{_datadir}/fish/vendor_completions.d/%{name}.fish
if [ ! -e $outfile ]; then
  $_bindir/%{name} --fish-completion-script %{name} | sed s/filenames/default/ > $outfile
else
  echo "$outfile exists!"; exit 1
fi

mkdir -p %{_mandir}/man1/
outfile=%{_mandir}/man1/%{name}.1
if [ ! -e $outfile ]; then
  help2man --no-info $_bindir/%{name} > $outfile
else
  echo "$outfile exists!"; exit 1
fi


%files
%{_datadir}/%{name}/runtime
%{_datadir}/bash-completion/completions/%{name}
%{_datadir}/zsh/site-functions/%{name}
%{_datadir}/fish/vendor_completions.d/%{name}
%{_mandir}/man1/%{name}.1*

%files common
%license LICENSE
%doc CHANGELOG.md README.md

%changelog
* Wed Jul  3 2024 Théophile Choutri <theophile@choutri.eu> - 0.1.0.0-1
- spec file generated by cabal-rpm-2.2.0
