# -*- coding: utf-8 -*-
from setuptools import setup

packages = \
['oreo']

package_data = \
{'': ['*']}

install_requires = \
['addict',
 'autoslot',
 'click',
 'hy>=1.0a4,<2.0',
 'hyrule',
 'more-itertools',
 'nixpkgs',
 'rich @ git+https://github.com/syvlorg/rich.git@master',
 'toolz']

setup_kwargs = {
    'name': 'oreo',
    'version': '1.0.0.0',
    'description': 'A bunch of useful funtions',
    'long_description': None,
    'author': 'sylvorg',
    'author_email': 'jeet.ray@syvl.org',
    'maintainer': None,
    'maintainer_email': None,
    'url': None,
    'packages': packages,
    'package_data': package_data,
    'install_requires': install_requires,
    'python_requires': '>=3.9,<4.0',
}


setup(**setup_kwargs)

