# -*- coding: utf-8 -*-
import scrapy


class TechcrunchproductlaunchSpider(scrapy.Spider):
    name = 'techcrunchProductLaunch'
    allowed_domains = ['https://techcrunch.com/search/product+launch#stq=product%20launch']
    start_urls = ['http://https://techcrunch.com/search/product+launch#stq=product%20launch/']

    def parse(self, response):
        pass
