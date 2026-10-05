#!/usr/bin/env python3
# Restore pinned Ubuntu native headers without installing outside the workspace.
from pathlib import Path
import hashlib,lzma,subprocess,urllib.request,concurrent.futures,sys
root=Path(sys.argv[1])
(root/"downloads").mkdir(parents=True,exist_ok=True)
base='https://snapshot.ubuntu.com/ubuntu/20260828T000000Z/'
for filename,suite in [('ubuntu-base-packages.xz','noble'),('ubuntu-packages.xz','noble-updates')]:
 path=root/'downloads'/filename
 if not path.exists():
  with urllib.request.urlopen(base+'dists/'+suite+'/main/binary-amd64/Packages.xz') as response:path.write_bytes(response.read())
packages={}
for filename in ['ubuntu-base-packages.xz','ubuntu-packages.xz']:
 for record in lzma.decompress((root/'downloads'/filename).read_bytes()).decode().split('\n\n'):
  fields={line.split(': ',1)[0]:line.split(': ',1)[1] for line in record.splitlines() if ': ' in line and not line.startswith(' ')}
  if 'Package' in fields:packages[fields['Package']]=fields
# Minimal replacement VMs may lack the runtime libraries as well as headers.
# Extract both from the same frozen Noble snapshot so development symlinks
# resolve without depending on an earlier VM's base-image package selection.
names=['libglib2.0-dev','libglib2.0-dev-bin','libglib2.0-0t64','libffi-dev','libffi8',
       'libpcre2-dev','libpcre2-8-0','libpcre2-16-0','libpcre2-32-0','libpcre2-posix3',
       'libmount-dev','libmount1','libblkid-dev','libblkid1','libicu-dev','libicu74',
       'zlib1g-dev','zlib1g','libpkgconf3','pkgconf-bin','pkgconf','pkg-config']
def fetch(name):
 p=packages[name];archive=root/'downloads'/Path(p['Filename']).name
 if not archive.exists():
  with urllib.request.urlopen(base+p['Filename']) as response:archive.write_bytes(response.read())
 if hashlib.sha256(archive.read_bytes()).hexdigest()!=p['SHA256']:raise ValueError('package checksum mismatch: '+name)
 return name,p['Version'],archive
results=list(concurrent.futures.ThreadPoolExecutor(max_workers=4).map(fetch,names))
for name,version,archive in results:
 subprocess.run(['dpkg-deb','-x',str(archive),str(root/'qemu-deps/sysroot')],check=True)
 print(name,version,flush=True)
# Linker-name symlinks to host runtime libraries can refer to absent extracted
# package targets. Materialize the exact host ABI files without installing them.
lib=root/'qemu-deps/sysroot/usr/lib/x86_64-linux-gnu'
for stem in ['icui18n','icuuc','icudata','glib-2.0','gthread-2.0','gobject-2.0','gio-2.0','ffi','mount','blkid','pcre2-8','z']:
 targets=sorted(Path('/usr/lib/x86_64-linux-gnu').glob('lib'+stem+'.so*'))
 valid=[p for p in targets if p.is_file()]
 if not valid:continue
 name=lib/('lib'+stem+'.so')
 if name.exists() or name.is_symlink():name.unlink()
 name.write_bytes(valid[-1].read_bytes())
