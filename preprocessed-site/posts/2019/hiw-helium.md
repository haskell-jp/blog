---
title: HIW 2019で発表された、HeliumというHaskell処理系について
subHeading: ～HIW 2019参加レポート その3～
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: September 20, 2019
tags: Haskell Implementors' Workshop
...
---

[前回](/posts/2019/hiw-ghc8.8.html)から引き続き、[Haskell Implementors' Workshop 2019](https://icfp19.sigplan.org/home/hiw-2019#About)への参加レポートとして、私の印象に残った発表を紹介します。  
今回から何回かは、「GHC 8.10に導入されるであろう機能」です。  
それぞれ役割が全く異なる

# Status Update on the Helium for Haskell compiler

Jurriaan HageUtrecht University, Netherlands

[Helium](https://github.com/Helium4Haskell/helium)という、プログラミングを初めて学習する人に向けて、Haskellの一部の機能をサポートしたコンパイラーの紹介と次期バージョンについて。  
エラーメッセージを優しくしたり、作成した内部DSLについての型エラーのメッセージをカスタマイズする機能を提供することで、プログラミング初学者が使いやすいように作られています。  
従来は独自の型クラスを定義する機能などを提供していなかったため、Haskellの標準にあまり準拠できていませんでしたが、次のバージョンとなる1.9ではそれらを実装し、その点を改善するそうです。  
将来的にはHaskell 2010のリファレンス実装となることを目指す
