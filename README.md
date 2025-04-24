# Executable Environment for OSF Project [4sjxz](https://osf.io/4sjxz/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Priming of Natural Scence Categorization during Continuous Flash Suppression [Data and Code]

**Project Description:**
> Continuous Flash Suppression (CFS) reduces conscious awareness of stimuli presented to one eye by flashing a sequence of high-contrast pattern masks to the other eye. However, it is currently unclear whether unconscious processing in CFS is limited to early sensory stages or extends to higher-level categorical or even semantic processes. Here, we approached this question using a priming paradigm with a large set of indoor and outdoor scene photographs. In each trial, participants were presented with a prime scene to the nondominant eye that was perceptually suppressed through CFS. The prime scene was followed by a visible target scene that was presented to both eyes. Participants rapidly reported whether the target scene showed an indoor or an outdoor setting. We found priming effects in the sense that participants’ responses were faster when prime and target scenes came from a congruent superordinate category (e.g., they were both outdoor scenes) and slower when they came from incongruent superordinate categories. Importantly, prime and target scenes always depicted different real-world scene contexts, thus minimizing the role of low-level feature relationships between prime and target scenes. The observed priming effects were modulated by prime visibility and stimulus onset asynchrony (SOA) of prime and target. The present results imply that processing of higher-level categorical information is possible during CFS, although some residual prime visibility is likely necessary for significant priming effects to occur.

**Original OSF Page:** [https://osf.io/4sjxz/](https://osf.io/4sjxz/)

---

**Important Note:** The contents of the `4sjxz_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_4sjxz/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_4sjxz/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `4sjxz_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

**Pull the Docker Image**

```bash
docker pull meet261/repo2docker-4sjxz:latest
```

**Launch RStudio Server**

```bash
docker run -e PASSWORD=yourpassword -p 8787:8787 meet261/repo2docker-4sjxz
```
Replace `yourpassword` with a secure password of your choice. You will use this to log in to the RStudio web interface.

**Once the container is running, visit `http://localhost:8787` in your browser.**
Use username: `rstudio` and the password you set with `-e PASSWORD=...`.
