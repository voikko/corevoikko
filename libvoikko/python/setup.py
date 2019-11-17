import setuptools

setuptools.setup(
    name="libvoikko",
    version="4.3",
    author="Harri Pitkänen",
    author_email="hatapitk@iki.fi",
    description="Python API for libvoikko, library of free natural language processing tools",
    url="https://github.com/voikko/corevoikko",
    packages=setuptools.find_packages(),
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Mozilla Public License 1.1 (MPL 1.1)",
        "License :: OSI Approved :: GNU General Public License v2 or later (GPLv2+)",
        "License :: OSI Approved :: GNU Lesser General Public License v2 or later (LGPLv2+)",
        "Operating System :: OS Independent"
    ],
    python_requires='>=3.2'
)
	
