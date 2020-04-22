"""
setup_setup.py

Author : Yusuke Kitamura
Create Date : 2020-04-22 21:08:18
Copyright (c) 2019- Yusuke Kitamura <ymyk6602@gmail.com>
"""
import sys
from pathlib import Path
import shutil
import subprocess


for i in range(3):
    val = input(f"Create Project '{sys.argv[1]}' at {Path('./').resolve()}/{sys.argv[1]}? [Y/n] : ")
    if val.lower() == "y":
        break
    elif val.lower() == "n":
        print("abort")
        sys.exit(1)
    print("You should enter 'Y' or 'n'")
else:
    print("abort")
    sys.exit(1)

templates_dir = Path.home() / "dotfiles/templates/"
project_name = sys.argv[1]
project_root_dir = Path(f"./{project_name}/")

if project_root_dir.exists():
    print("project {project_name} is already exists.")
    sys.exit(1)

(project_root_dir / f"src/{project_name}").mkdir(parents=True, exist_ok=True)
(project_root_dir / f"tests").mkdir(exist_ok=True)

# setup setup.py
setup_template = templates_dir / "setup.py"

with open(setup_template, "r") as f:
    template = f.read()
    template = template.format(project_name)

setup_py = project_root_dir / "setup.py"
with open(setup_py, "w") as f:
    f.write(template)

# requirements.txt
(project_root_dir / "requirements.txt").touch()

# setup __init__.py
init_template = templates_dir / "__init__.py"
init_py = project_root_dir / f"src/{project_name}/__init__.py"
shutil.copy(init_template, init_py)

# setup pytest.ini
pytest_template = templates_dir / "pytest.ini"
pytest_ini = project_root_dir / "tests/pytest.ini"
shutil.copy(pytest_template, pytest_ini)

sys.exit(0)
