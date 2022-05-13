# Brahma.FSharp

---

## What is Brahma.FSharp?

Brahma.FSharp is a tool for GPGPUs utilization in your F# projects. It is based on F# quotations to OpenCL translation. Brahma.FSharp provides not only translator but also memory management system and host-devise communication system.

## Why use Brahma.FSharp?

 * Brahma.FSharp uses OpenCL for communication with GPU. So, you can work not only with NVIDIA hardware but with any device, which supports OpenCL (e.g. with AMD or Intel devices).
 * Brahma.FSharp supports not only primitive types, but also tuples, structures, and discriminated unions.
 * Brahma.FSharp not limited by imperative subset of F#. It supports nested bindings, pattern matching.
 * Communication with GPGPUs based on Mailbox processors, thus asynchronous communications, which are native for GPGPU computations, can be used easily.
 
---

<div class="row row-cols-1 row-cols-md-2">
  <div class="col mb-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Tutorials</h5>
        <p class="card-text">Takes you by the hand through a series of steps to create your first thing. </p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Tutorials/Getting_Started.html" class="btn btn-primary">Get started</a>
      </div>
    </div>
  </div>
  <div class="col mb-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">How-To Guides</h5>
        <p class="card-text">Guides you through the steps involved in addressing key problems and use-cases. </p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/How_Tos/Doing_A_Thing.html" class="btn btn-primary">Learn Usecases</a>
      </div>
    </div>
  </div>
  <div class="col mb-4 mb-md-0">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Explanations</h5>
        <p class="card-text">Discusses key topics and concepts at a fairly high level and provide useful background information and explanation.</p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Explanations/Background.html" class="btn btn-primary">Dive Deeper</a>
      </div>
    </div>
  </div>
  <div class="col">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Api Reference</h5>
        <p class="card-text">Contain technical reference for APIs.</p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Api_Reference/Brahma.FSharp/Brahma.FSharp.OpenCL.Translator/Brahma.FSharp.OpenCL.Translator.html" class="btn btn-primary">Read Api Docs</a>
      </div>
    </div>
  </div>
</div>
