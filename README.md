![The Harmony Project logo](https://raw.githubusercontent.com/harmonydata/brand/main/Logo/PNG/%D0%BB%D0%BE%D0%B3%D0%BE%20%D1%84%D1%83%D0%BB-05.png)

<a href="https://harmonydata.ac.uk"><span align="left">🌐 harmonydata.ac.uk</span></a>
<a href="https://www.linkedin.com/company/harmonydata"><img align="left" src="https://raw.githubusercontent.com//harmonydata/.github/main/profile/linkedin.svg" alt="Harmony | LinkedIn" width="21px"/></a>
<a href="https://twitter.com/harmony_data"><img align="left" src="https://raw.githubusercontent.com//harmonydata/.github/main/profile/x.svg" alt="Harmony | X" width="21px"/></a>
<a href="https://www.instagram.com/harmonydata/"><img align="left" src="https://raw.githubusercontent.com//harmonydata/.github/main/profile/instagram.svg" alt="Harmony | Instagram" width="21px"/></a>
<a href="https://www.facebook.com/people/Harmony-Project/100086772661697/"><img align="left" src="https://raw.githubusercontent.com//harmonydata/.github/main/profile/fb.svg" alt="Harmony | Facebook" width="21px"/></a>
<a href="https://www.youtube.com/channel/UCraLlfBr0jXwap41oQ763OQ"><img align="left" src="https://raw.githubusercontent.com//harmonydata/.github/main/profile/yt.svg" alt="Harmony | YouTube" width="21px"/></a>

 [![Harmony on Twitter](https://img.shields.io/twitter/follow/harmony_data.svg?style=social&label=Follow)](https://twitter.com/harmony_data) 


# Harmony example scripts

Harmony is a tool using AI which allows you to compare items from questionnaires and identify similar content. You can try Harmony at https://harmonydata.ac.uk/app and you can read our blog at https://harmonydata.ac.uk/blog/.

Here you can find example scripts for using the Python and R libraries.

## R examples

* Walkthrough R notebook in R Studio: <a href="https://harmonydata.ac.uk/harmony_r_example.nb.html" target="_parent"><img src="https://img.shields.io/badge/RStudio-4285F4" alt="Open In R Studio"/></a>
* Walkthrough R notebook in Google Colab: <a href="https://colab.research.google.com/github/harmonydata/experiments/blob/main/Harmony_R_example.ipynb" target="_parent"><img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/></a> [View the PDF documentation of the R package on CRAN](https://cran.r-project.org/web/packages/harmonydata/harmonydata.pdf)


## Python examples

* Walkthrough Python notebook in [Google Colab](https://colab.research.google.com/github/harmonydata/harmony/blob/main/Harmony_example_walkthrough.ipynb) with a single click: <a href="https://colab.research.google.com/github/harmonydata/harmony/blob/main/Harmony_example_walkthrough.ipynb" target="_parent"><img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/></a>
* [Example script to create a crosswalk table on real survey data](./create_harmony_crosswalks_in_python.ipynb)
* [Example script to strip prefixes from questions](./strip_common_prefixes_from_questions.ipynb)



## 🖱 Looking to try Harmony in the browser?

Visit: https://harmonydata.ac.uk/app/

## Looking for Harmony source code?

Harmony is based on four repositories: the core Python library, the API, the R port, and the front end for running in the browser. We welcome contributions and you can raise issues, pick up issues, and make pull requests.

   * [Python](https://github.com/harmonydata/harmony) - the main core library and the Python package which is on [Pypi](https://pypi.org/project/harmonydata/)
   * [R](https://github.com/harmonydata/harmony_r) - the R port is on [CRAN](https://cran.r-project.org/web/packages/harmonydata/index.html) and it is slightly less mature than Python so we really appreciate if you can give the R package some TLC.
   * [API](https://github.com/harmonydata/harmonyapi) - the Python API runs with Pydantic and Fast API and is running on an on-prem server enabling the web app to work
   * [Web front end](https://github.com/harmonydata/app) - we welcome feedback and contributions on front end and UX issues
* If you're doing research and found Harmony useful, please [cite us](/ai-in-mental-health/bmc-psychiatry-paper/)!
* If you're a researcher trying to use the tool, and you encounter a problem, a bug, or a feature which you would like us to implement, please [raise an issue on Github](https://github.com/harmonydata/harmony) or [message us on Discord](https://discord.gg/harmonydata).

[Read our guide to contributing to Harmony here](https://harmonydata.ac.uk/contributing-to-harmony/) or read [CONTRIBUTING.md](https://github.com/harmonydata/harmony/blob/main/CONTRIBUTING.md).

## 🖥 Installation instructions for Python library (video)

[![Installing Harmony](https://raw.githubusercontent.com/harmonydata/.github/main/profile/installation_video.jpg)](https://www.youtube.com/watch?v=enWh0-4I0Sg "Installing Harmony")


## Who to contact?

You can contact Harmony team at https://harmonydata.ac.uk/, or Thomas Wood at https://fastdatascience.com/.


## ‎😃💁 Who worked on Harmony?

Harmony is a collaboration project between [Ulster University](https://ulster.ac.uk/), [University College London](https://ucl.ac.uk/), the [Universidade Federal de Santa Maria](https://www.ufsm.br/), and [Fast Data Science](http://fastdatascience.com/).  Harmony is funded by [Wellcome](https://wellcome.org/) as part of the [Wellcome Data Prize in Mental Health](https://wellcome.org/grant-funding/schemes/wellcome-mental-health-data-prize).

The core team at Harmony is made up of:

* [Dr Bettina Moltrecht, PhD](https://profiles.ucl.ac.uk/60736-bettina-moltrecht) (UCL)
* [Dr Eoin McElroy](https://www.ulster.ac.uk/staff/e-mcelroy) (University of Ulster)
* [Dr George Ploubidis](https://profiles.ucl.ac.uk/48171-george-ploubidis) (UCL)
* [Dr Mauricio Scopel Hoffmann](https://ufsmpublica.ufsm.br/docente/18264) (Universidade Federal de Santa Maria, Brazil)
* [Thomas Wood](https://freelancedatascientist.net/) ([Fast Data Science](https://fastdatascience.com))



## 📜 License

MIT License. Copyright (c) 2023 Ulster University (https://www.ulster.ac.uk)

## 📜 How do I cite Harmony?

You can cite our validation paper:

 McElroy, Wood, Bond, Mulvenna, Shevlin, Ploubidis, Scopel Hoffmann, Moltrecht, [Using natural language processing to facilitate the harmonisation of mental health questionnaires: a validation study using real-world data](https://bmcpsychiatry.biomedcentral.com/articles/10.1186/s12888-024-05954-2#citeas). BMC Psychiatry 24, 530 (2024), https://doi.org/10.1186/s12888-024-05954-2
 

A BibTeX entry for LaTeX users is

```
@article{mcelroy2024using,
  title={Using natural language processing to facilitate the harmonisation of mental health questionnaires: a validation study using real-world data},
  author={McElroy, Eoin and Wood, Thomas and Bond, Raymond and Mulvenna, Maurice and Shevlin, Mark and Ploubidis, George B and Hoffmann, Mauricio Scopel and Moltrecht, Bettina},
  journal={BMC psychiatry},
  volume={24},
  number={1},
  pages={530},
  year={2024},
  publisher={Springer}
}
```

