---
title: Web fingerprinting is worse than I thought
description: Discussing current state of web fingerprinting and how to protect yourself
---
If you are reading this article, you are most likely using a web browser, and you have some expectations or beliefs about online privacy and security. For example, I do not know what you are reading on other tabs on your web browser, and you would like to keep it that way. But the websites themselves know that you are reading a particular page on their website. They most likely know your IP address and if you are signed in to their website, they also know your identity. This is not unreasonable because you chose to identify yourself in exchange for certain services. That’s how web works.

You might also be heard about cross site tracking using cookies. Cookies are persistent files set on your web browser by a website to identify you later when you visit the same site. Cross site cookies are set by third-party domains present on a website, and the same third-party might also present in other websites as well. Third party domains track you across your browsing sessions and able to identify you uniquely across different websites. That’s how you are shown ads based on your browsing history. Because the third party is usually an advertising company (cough! Google) and they are present in almost all websites. Even though it seems unethical for a third party to track your browsing history, at-least you had control. Web Browsers allows you to delete cookies, so third parties cannot link you back to your past sessions. This is what Private Browsing does. It basically wipes all cookies (and history) upon closing the window.

Browsers like Firefox now ships with advanced protection against this kind of tracking. They isolate third party cookies per website. This means advertisers or third-parties cannot track you across different websites. This affects advertisement companies revenue because they cannot know your full browsing activity and hence cannot show you personalized ads.

Based on your threat model, even being identified by a first party website across different sessions might be uncomfortable for you. So you might set your web browser to automatically clear cookies or use add-ons to do that.

But companies found another way to uniquely identify you across different sessions and websites without using cookies or other persistent storage. It’s called web fingerprinting. Fingerprinting is a more sophisticated approach to identify a user among millions of others. It works by studying your web browser and hardware configuration. Many websites use a fingerprinting library to generate a unique ID. This library collects data from multiple JavaScript APIs offered by your web browser. For example, websites can see web browser version, number of CPUs on your device, screen size, number of touchpoints, video/audio codecs, operating system and many other details that you would not want a typical news website to see.

All of these values are combined to generate a unique ID. Surprisingly, each user’s device and browser specifications differ so much that they get a unique ID among millions.

I did not think web fingerprinting is serious until I came across a company which is actually selling fingerprinting as a service to other websites. I tried their demo and shocked how accurate it is. Many ecommerce websites use it because these fingerprinting companies sell it, saying it prevents credit card frauds and increases security of the websites.

If you are paranoid like me and use private browsers like Firefox Focus or always clearing cookies when you close the browser, it doesn’t really help to protect your privacy. Web Browsers and Web Standards become so complicated that fingerprinting is easier than you think.

## Fingerprinting as a Service

We are going to test a product built by a company called FingerprintJS Inc. who is selling fingerprinting as a service. They make JavaScript fingerprinting libraries which are in fact open source and sell it to many websites. There’s FingerprintJS Pro which is an even scarier version of regular fingerprinting library. It doesn’t matter if you are using a VPN or Private Browsing mode, they can accurately identify you. Here’s how they are describing themselves, **“The device identity platform for high-scale applications”.**

<img class="img-responsive" src="/images/2023-03-19-fingerprinting/fingerprint.com.webp" alt="Fingerprint.com features" />

FingerprintJS has a demo built into it's homepage, [https://fingerprint.com](https://fingerprint.com). When you visit this website, they generate a visitor ID (fingerprint) which is unique for your browser. So even if you clear the cache (and other site data) or visit the site in Private Browsing mode, they can generate the same ID and correlate with your previous visit.

## My tests on popular web browsers

Now we are going to perform the following steps to prove that fingerprinting works and severely undermines our privacy.

**Step 1:** Visit [https://fingerprint.com](https://fingerprint.com)

**Step 2:** View the fingerprint generated.

**Step 3:** Clear browser cache and all other site data.
 
**Step 4:** Visit [https://fingerprint.com](https://fingerprint.com) once again. 

**Step 5:** View the fingerprint and also the previous visit history. Even if the browser has no cookies or other site data, their product can generate the same visitor ID and link it back to our previous visit.

**Step 6:** Clear browser cache and all other site data.

**Step 7:** Visit [https://fingerprint.com](https://fingerprint.com) in Private Browsing mode.

**Step 8:** View the fingerprint and see how it is being correlated to the previous two visits we already made. Yes, in Private Browsing mode.

We are going to perform these tests on Firefox, Chromium, and Tor Browser.

### Firefox

Visit 1 | Visit 2 | Private Browsing
---|---|---
[<img class="img-responsive" src="/images/2023-03-19-fingerprinting/firefox_visit_1.webp" alt="Visit 1 to fingerprint.com on Firefox" />](/images/2023-03-19-fingerprinting/firefox_visit_1.webp) | [<img class="img-responsive" src="/images/2023-03-19-fingerprinting/firefox_visit_2.webp" alt="Visit 2 to fingerprint.com on Firefox" />](/images/2023-03-19-fingerprinting/firefox_visit_2.webp) | [<img class="img-responsive" src="/images/2023-03-19-fingerprinting/firefox_visit_private_mode.webp" alt="Visit to fingerprint.com on Firefox on Private Browsing mode" />](/images/2023-03-19-fingerprinting/firefox_visit_private_mode.webp)

Notice how different sessions are connected by the same fingerprint generated by FingerprintJS. Firefox in its default configuration is prone to fingerprinting.

### Firefox with privacy.resistFingerprinting = true

Firefox has a setting called resistFingerprinting (initially contributed by The Tor Project) that makes it more resistance to fingerprinting. When activated, Firefox tries to mask certain properties like User Agent, CPU Count, Timezone, Screen Resolution etc. uniform for all users. This makes it harder for fingerprinting.

You can enable it by visiting `about:config` and setting `privacy.resistFingerprinting = true` in your Firefox browser.

Visit 1 | Visit 2 | Private Browsing
---|---|---
[<img class="img-responsive" src="/images/2023-03-19-fingerprinting/firefox_resistfingerprinting_visit_1.webp" alt="Visit 1 to fingerprint.com on Firefox with resistFingerprinting set to true" />](/images/2023-03-19-fingerprinting/firefox_resistfingerprinting_visit_1.webp) | [<img class="img-responsive" src="/images/2023-03-19-fingerprinting/firefox_resistfingerprinting_visit_2.webp" alt="Visit 2 to fingerprint.com on Firefox with resistFingerprinting set to true" />](/images/2023-03-19-fingerprinting/firefox_resistfingerprinting_visit_2.webp) | [<img class="img-responsive" src="/images/2023-03-19-fingerprinting/firefox_resistfingerprinting_visit_private_mode.webp" alt="Visit to fingerprint.com on Firefox with resistFingerprinting set to true on Private Browsing mode" />](/images/2023-03-19-fingerprinting/firefox_resistfingerprinting_visit_private_mode.webp)

This time, FingerprintJS could not link it with previous sessions. Each 
session gets a unique ID since Firefox hardens certain APIs against 
fingerprinting.

### Chromium / Chrome

Chromium (Chrome) is built by Google, an advertisement company which  tracks its users for showing relevant ads. So naturally it doesn’t have any inbuilt protection against fingerprinting. Chromium (and Google Chrome) is vulnerable to fingerprinting.

Visit 1 | Visit 2 | Private Browsing
---|---|---
[<img class="img-responsive" src="/images/2023-03-19-fingerprinting/chromium_visit_1.webp" alt="Visit 1 to fingerprint.com on Chromium" />](/images/2023-03-19-fingerprinting/chromium_visit_1.webp) | [<img class="img-responsive" src="/images/2023-03-19-fingerprinting/chromium_visit_2.webp" alt="Visit 2 to fingerprint.com on Chromium" />](/images/2023-03-19-fingerprinting/chromium_visit_2.webp) | [<img class="img-responsive" src="/images/2023-03-19-fingerprinting/chromium_visit_private_mode.webp" alt="Visit to fingerprint.com on Chromium Private Browsing mode" />](/images/2023-03-19-fingerprinting/chromium_visit_2.webp) 

FingerprintJS generates the same ID in each Chromium session, thus it can identify its users across different sessions.

### Tor Browser

Tor Browser is made by [The Tor Project](https://www.torproject.org/), a non-profit organization. Tor Browser routes internet traffic through 
multiple relays across the world, thus making user’s browsing sessions 
more private. It is based on Firefox and many features of Tor Browser 
has been incorporated back in Firefox.

Visit 1 | Visit 2
---|---|---
[<img class="img-responsive" src="/images/2023-03-19-fingerprinting/torbrowser_visit_1.webp" alt="Visit 1 to fingerprint.com on Tor Browser" />](/images/2023-03-19-fingerprinting/torbrowser_visit_1.webp) | [<img class="img-responsive" src="/images/2023-03-19-fingerprinting/torbrowser_visit_2.webp" alt="Visit 2 to fingerprint.com on Tor Browser" />](/images/2023-03-19-fingerprinting/torbrowser_visit_2.webp) |


> Please note that Tor Browser always operates in Private Browsing mode. So I did not test it under Private Browsing explicitly.

FingerprintJS could not link two different Tor Browser sessions by the same user. So Tor Browser is more secure against fingerprinting.


## Conclusion

Fingerprinting has become a popular method of user tracking due to its ability to connect multiple different browsing sessions even if the user clears browsing history and data. Given there are companies selling fingerprinting as a service, if you want to really protect yourself from fingerprinting, you should use Tor Browser or Firefox with `resistFingerprinting=true`. If you need to use Chromium, then Brave browser is a good choice. It also randomizes fingerprint for each session, making it harder to link your browsing sessions. However, I do not recommend Brave because it is based on Google’s Chromium engine, thus only encourages Google’s monopoly.

On mobile, only Tor Browser and Firefox with `resistFingerprinting=true` were able to protect against fingerprinting. Firefox Focus leaks fingerprints even if you clear its session each time. Also note that VPNs does not help with fingerprinting. They only masks IP address.
